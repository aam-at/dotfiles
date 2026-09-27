/*
 * wellbeing on Linux (Hyprland, niri, COSMIC, and other desktops through
 * commands set in the dashboard): nudges are freedesktop notifications
 * (notify-send); what's in front and idle time come from ActivityWatch's own
 * watchers, since Wayland has no portable way to ask; Do Not Disturb and
 * grayscale are commands, with defaults for DankMaterialShell, noctalia,
 * COSMIC and Hyprland. The loop sleeps until the next thing is due.
 * Signals: SIGUSR2 toggles focus mode and SIGRTMIN bedtime (--focus and
 * --bedtime send them); SIGUSR1 reloads the settings at once.
 */
#include <fcntl.h>
#include <signal.h>
#include <sys/file.h>
#include <sys/stat.h>
#include <unistd.h>

#define PLATFORM_ICON_TIMER "\xE2\x8F\xB1" /* ⏱ */
#define PLATFORM_ICON_FOCUS "\xE2\x98\xBE" /* ☾ */

static void core_settings(void);
static void core_usage(void);
static void core_check_focus(void);
static void core_start(void);
static void core_stop(void);

static volatile sig_atomic_t reload_requested, stop_requested, focus_requested, bedtime_requested;
static int watching_focus;

static double platform_seconds(void) {
    struct timespec now;
    clock_gettime(CLOCK_MONOTONIC, &now);
    return (double)now.tv_sec + now.tv_nsec / 1e9;
}

/* Not portable under Wayland: the afk watcher decides instead. */
static double platform_idle_seconds(void) {
    return -1;
}

/* Likewise: the window watcher's latest event says what's in front. */
static int platform_foreground(char *app, size_t size) {
    (void)app, (void)size;
    return 0;
}

/* The log in $XDG_STATE_HOME; the status and pid files in
   $XDG_RUNTIME_DIR, which doesn't outlive the session. */
static void platform_data_path(const char *name, char *out, size_t size) {
    const char *home = getenv("HOME") ? getenv("HOME") : "/tmp";
    if (strcmp(name, "wellbeing.log") == 0) {
        if (getenv("XDG_STATE_HOME")) snprintf(out, size, "%s", getenv("XDG_STATE_HOME"));
        else snprintf(out, size, "%s/.local/state", home);
        mkdir(out, 0755);
        snprintf(out + strlen(out), size - strlen(out), "/%s", name);
        return;
    }
    const char *runtime = getenv("XDG_RUNTIME_DIR");
    if (runtime) snprintf(out, size, "%s/%s", runtime, name);
    else snprintf(out, size, "/tmp/%s-%d", name, (int)getuid());
}

static void platform_write_stdout(const char *text) {
    fputs(text, stdout);
    fflush(stdout);
}

/* argv, detached; children are reaped by SIGCHLD being ignored. */
static void spawn(char *const argv[]) {
    if (fork() != 0) return;
    setsid();
    execvp(argv[0], argv);
    _exit(127);
}

static void platform_run(const char *command) {
    char *argv[] = {"/bin/sh", "-c", (char *)command, NULL};
    spawn(argv);
}

static void platform_notify(const char *title, const char *body, double progress, int urgent) {
    char value[32];
    char *argv[16] = {"notify-send", "-a", "Wellbeing", "-i", "appointment-soon", "-u", urgent ? "critical" : "normal"};
    int n = 7;
    if (progress >= 0) {
        /* A progress bar, where the notification daemon shows one. */
        snprintf(value, sizeof value, "int:value:%d", (int)(progress > 1 ? 100 : progress * 100));
        argv[n++] = "-h", argv[n++] = value;
    }
    argv[n++] = (char *)title, argv[n++] = (char *)body, argv[n] = NULL;
    spawn(argv);
}

static int platform_grayscale(int on) {
    (void)on;
    return 0;
}

static void exe_dir(char *out, size_t size) {
    ssize_t length = readlink("/proc/self/exe", out, size - 1);
    out[length > 0 ? length : 0] = 0;
    char *slash = strrchr(out, '/');
    if (slash) *slash = 0;
}

/* Commands for the desktop in use, where the dashboard leaves them empty. */
static void platform_default_commands(Config *c) {
    const char *desktop = getenv("XDG_CURRENT_DESKTOP");
    if (desktop && strstr(desktop, "COSMIC")) {
        /* cosmic-notifications watches its config file. */
        const char *file = "d=\"${XDG_CONFIG_HOME:-$HOME/.config}/cosmic/com.system76.CosmicNotifications/v1\"; mkdir -p \"$d\" && echo %s > \"$d/do_not_disturb\"";
        if (!*c->dnd_on) snprintf(c->dnd_on, sizeof c->dnd_on, file, "true");
        if (!*c->dnd_off) snprintf(c->dnd_off, sizeof c->dnd_off, file, "false");
    }
    else {
        /* DankMaterialShell, else noctalia: whichever shell is running. */
        if (!*c->dnd_on)
            snprintf(c->dnd_on, sizeof c->dnd_on, "dms ipc call notifications enableDoNotDisturbIndefinitely 2>/dev/null || noctalia msg notification-dnd-set on");
        if (!*c->dnd_off)
            snprintf(c->dnd_off, sizeof c->dnd_off,
                "if [ \"$(dms ipc call notifications getDoNotDisturb 2>/dev/null)\" = true ]; then dms ipc call notifications toggleDoNotDisturb; "
                "else noctalia msg notification-dnd-set off; fi");
    }
    if (getenv("HYPRLAND_INSTANCE_SIGNATURE")) {
        char dir[512];
        exe_dir(dir, sizeof dir);
        if (!*c->grayscale_on) snprintf(c->grayscale_on, sizeof c->grayscale_on, "hyprctl keyword decoration:screen_shader '%s/grayscale.frag'", dir);
        if (!*c->grayscale_off) snprintf(c->grayscale_off, sizeof c->grayscale_off, "hyprctl keyword decoration:screen_shader '[[EMPTY]]'");
    }
}

static void platform_watch_focus(int on) {
    watching_focus = on;
}

/* ---- The running helper ---- */

static int running_pid(void) {
    char path[512];
    platform_data_path("wellbeing.pid", path, sizeof path);
    FILE *file = fopen(path, "r");
    int pid = 0;
    if (file) {
        if (fscanf(file, "%d", &pid) != 1) pid = 0;
        fclose(file);
    }
    return pid > 0 && kill(pid, 0) == 0 ? pid : 0;
}

static int platform_running(void) {
    return running_pid() != 0;
}

static void platform_send(Command command) {
    int pid = running_pid();
    if (pid) kill(pid, command == COMMAND_FOCUS ? SIGUSR2 : SIGRTMIN);
}

/* The next pass of the loop: notify-send already runs on its own. */
static void platform_sync_soon(void) {
    reload_requested = 1;
}

static void on_signal(int signal) {
    if (signal == SIGUSR1) reload_requested = 1;
    else if (signal == SIGUSR2) focus_requested = 1;
    else if (signal == SIGRTMIN) bedtime_requested = 1;
    else stop_requested = 1;
}

static int platform_daemon(void) {
    /* One instance: the pid file stays locked while this runs. */
    char path[512];
    platform_data_path("wellbeing.pid", path, sizeof path);
    int lock = open(path, O_RDWR | O_CREAT, 0644);
    if (lock < 0 || flock(lock, LOCK_EX | LOCK_NB) != 0) return 0;
    if (ftruncate(lock, 0) == 0) dprintf(lock, "%d\n", (int)getpid());

    /* No SA_RESTART: a signal cuts the sleep short. */
    struct sigaction action = {.sa_handler = on_signal};
    sigaction(SIGUSR1, &action, NULL);
    sigaction(SIGUSR2, &action, NULL);
    sigaction(SIGRTMIN, &action, NULL);
    sigaction(SIGTERM, &action, NULL);
    sigaction(SIGINT, &action, NULL);
    sigaction(SIGHUP, &action, NULL);
    signal(SIGCHLD, SIG_IGN);

    core_start();
    double now = platform_seconds();
    double next_settings = now + SETTINGS_EVERY_S, next_usage = now + USAGE_EVERY_S, next_focus = now + FOCUS_POLL_S;
    while (!stop_requested) {
        if (reload_requested) reload_requested = 0, core_settings();
        if (focus_requested) focus_requested = 0, core_toggle_focus();
        if (bedtime_requested) bedtime_requested = 0, core_toggle_bedtime();
        now = platform_seconds();
        if (now >= next_settings) core_settings(), next_settings = now + SETTINGS_EVERY_S;
        if (now >= next_usage) core_usage(), next_usage = now + USAGE_EVERY_S;
        if (watching_focus && now >= next_focus) core_check_focus(), next_focus = now + FOCUS_POLL_S;
        double wake = next_settings < next_usage ? next_settings : next_usage;
        if (watching_focus && next_focus < wake) wake = next_focus;
        double wait = wake - platform_seconds();
        if (wait > 0) {
            struct timespec sleep = {(time_t)wait, (long)((wait - (time_t)wait) * 1e9)};
            nanosleep(&sleep, NULL);
        }
    }
    core_stop();
    unlink(path);
    return 0;
}

static int wellbeing_main(int argc, char **argv);

int main(int argc, char **argv) {
    return wellbeing_main(argc, argv);
}
