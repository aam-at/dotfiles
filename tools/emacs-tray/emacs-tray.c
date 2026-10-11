/*
 * Tray icons for the Doom and Spacemacs daemons, the Linux counterpart of
 * windots' yasb/emacs/emacs-tray.c. The shell's tray (noctalia, DMS) lists
 * them next to the other tray apps. Each icon is the profile's official logo:
 * gray when the daemon is not running, rotating while it starts, full color
 * when it is up. Left-click opens a frame; right-click offers Open, Start,
 * Stop and Restart (Open and Restart start a stopped daemon).
 *
 * State is systemd's for emacs@<profile>.service (config/systemd): the unit
 * is "activating" for as long as Emacs loads, about a minute for Doom. The
 * actions run scripts/emacs-daemon.
 *
 * Each icon is a StatusNotifierItem with a com.canonical.dbusmenu menu, at
 * /StatusNotifierItem/<profile> and /MenuBar/<profile>: noctalia takes two
 * items from one process only when their object paths differ.
 *
 * Icons: icons/emacs-tray-<profile>.png, -off.png, -spin0..11.png, beside the
 * binary. They are windots' .ico files as PNG.
 * Build: make (needs libsystemd); make install puts it in ~/.local/lib.
 * Tests: emacs-tray.test.c (tools/test.sh runs them).
 */
#ifdef __linux__
#include <limits.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

enum { STOPPED, STARTING, RUNNING };
enum { ICON_COLOR, ICON_OFF, ICON_SPIN };
enum { ITEM_OPEN = 1, ITEM_START, ITEM_STOP, ITEM_RESTART, ITEM_COUNT };

#define FRAMES 12
#define FRAME_MS 80

/* From the unit's ActiveState: a forking unit is "activating" until the daemon
   has loaded, and "deactivating" or "failed" count as not running. */
static int state_from_active(const char *active) {
    if (!strcmp(active, "active") || !strcmp(active, "reloading")) return RUNNING;
    if (!strcmp(active, "activating")) return STARTING;
    return STOPPED;
}

/* Which icon to show: color when up, gray when not running, a spin frame by the clock
 * while starting. */
static int icon_index(int state, uint64_t tick_ms) {
    if (state == RUNNING) return ICON_COLOR;
    if (state == STOPPED) return ICON_OFF;
    return ICON_SPIN + (int)(tick_ms / FRAME_MS % FRAMES);
}

/* The icon's file name without ".png". The prefix keeps the shell from finding
   an icon theme's own "doom" first. */
static void icon_name(const char *profile, int index, char *out, size_t size) {
    if (index == ICON_COLOR)
        snprintf(out, size, "emacs-tray-%s", profile);
    else if (index == ICON_OFF)
        snprintf(out, size, "emacs-tray-%s-off", profile);
    else
        snprintf(out, size, "emacs-tray-%s-spin%d", profile, index - ICON_SPIN);
}

/* Open and Restart wait out a start; Start needs a stopped daemon, Stop a running one. */
static int item_enabled(int item, int state) {
    if (item == ITEM_START) return state == STOPPED;
    if (item == ITEM_STOP) return state == RUNNING;
    return state != STARTING;
}

#ifndef EMACS_TRAY_TEST
#include <systemd/sd-bus.h>
#include <systemd/sd-event.h>
#include <sys/wait.h>
#include <time.h>
#include <unistd.h>

#define SYSTEMD "org.freedesktop.systemd1"
#define WATCHER "org.kde.StatusNotifierWatcher"
#define ITEM_INTERFACE "org.kde.StatusNotifierItem"
#define MENU_INTERFACE "com.canonical.dbusmenu"

static const char *profiles[] = {"doom", "spacemacs"};
static const char *titles[] = {"Doom Emacs", "Spacemacs"};
static const char *status_words[] = {"not running", "starting", "running"};
static const char *actions[] = {NULL, "open", "start", "stop", "restart"};
static const char *labels[] = {NULL, "Open", "Start", "Stop", "Restart"};

typedef struct {
    int which;
    char item_path[64], menu_path[64];
    char *unit_path; /* the systemd unit's object */
    int state, shown;
    uint32_t revision; /* of the menu: bumped when items change */
} Item;

static Item items[2];
static sd_bus *bus;
static char icon_dir[PATH_MAX];
static sd_event_source *timer;

static uint64_t now_ms(void) {
    struct timespec now;
    clock_gettime(CLOCK_MONOTONIC, &now);
    return (uint64_t)now.tv_sec * 1000 + (uint64_t)now.tv_nsec / 1000000;
}

/* scripts/emacs-daemon, detached: "open" lives as long as its frame. */
static void run_action(const char *action, const char *profile) {
    pid_t child = fork();
    if (child > 0) waitpid(child, NULL, 0);
    if (child != 0) return;
    if (fork() != 0) _exit(0);
    setsid();
    char path[PATH_MAX];
    const char *home = getenv("HOME");
    snprintf(path, sizeof path, "%s/.local/bin/emacs-daemon", home ? home : "");
    execl(path, "emacs-daemon", action, profile, (char *)NULL);
    execlp("emacs-daemon", "emacs-daemon", action, profile, (char *)NULL);
    _exit(127);
}

static int unit_state(Item *item) {
    char *active = NULL;
    int state = STOPPED;
    if (sd_bus_get_property_string(bus, SYSTEMD, item->unit_path, SYSTEMD ".Unit",
                                   "ActiveState", NULL, &active) >= 0)
        state = state_from_active(active);
    free(active);
    return state;
}

/* Tells the tray what changed: the tooltip and menu with the state, the icon with the
   state or the spin frame. The timer runs only while a daemon is starting. */
static void refresh(Item *item) {
    int state = unit_state(item);
    int index = icon_index(state, now_ms());
    if (state != item->state) {
        item->state = state;
        item->revision++;
        sd_bus_emit_signal(bus, item->item_path, ITEM_INTERFACE, "NewToolTip", NULL);
        sd_bus_emit_signal(bus, item->menu_path, MENU_INTERFACE, "LayoutUpdated", "ui",
                           item->revision, 0);
    }
    if (index != item->shown) {
        item->shown = index;
        sd_bus_emit_signal(bus, item->item_path, ITEM_INTERFACE, "NewIcon", NULL);
    }
    if (state == STARTING) {
        sd_event_source_set_time_relative(timer, FRAME_MS * 1000);
        sd_event_source_set_enabled(timer, SD_EVENT_ONESHOT);
    }
}

static int on_tick(sd_event_source *source, uint64_t usec, void *userdata) {
    (void)source, (void)usec, (void)userdata;
    for (int i = 0; i < 2; i++) refresh(&items[i]);
    return 0;
}

static int on_unit_changed(sd_bus_message *message, void *userdata, sd_bus_error *error) {
    (void)message, (void)error;
    refresh(userdata);
    return 0;
}

/* By object path: the tray pairs it with this connection. */
static void register_items(void) {
    for (int i = 0; i < 2; i++)
        sd_bus_call_method_async(bus, NULL, WATCHER, "/StatusNotifierWatcher", WATCHER,
                                 "RegisterStatusNotifierItem", NULL, NULL, "s",
                                 items[i].item_path);
}

/* The shell (re)started and lost every icon: add them again. */
static int on_watcher_owner(sd_bus_message *message, void *userdata, sd_bus_error *error) {
    (void)userdata, (void)error;
    const char *name, *old_owner, *new_owner;
    if (sd_bus_message_read(message, "sss", &name, &old_owner, &new_owner) >= 0 &&
        *new_owner)
        register_items();
    return 0;
}

/* ---- org.kde.StatusNotifierItem ---- */

static int item_property(sd_bus *connection, const char *path, const char *interface,
                         const char *property, sd_bus_message *reply, void *userdata,
                         sd_bus_error *error) {
    (void)connection, (void)path, (void)interface, (void)error;
    Item *item = userdata;
    const char *profile = profiles[item->which];
    char text[96] = "";
    if (!strcmp(property, "WindowId")) return sd_bus_message_append(reply, "i", 0);
    if (!strcmp(property, "ItemIsMenu")) return sd_bus_message_append(reply, "b", 0);
    if (!strcmp(property, "Menu"))
        return sd_bus_message_append(reply, "o", item->menu_path);
    if (strstr(property, "Pixmap")) return sd_bus_message_append(reply, "a(iiay)", 0);
    if (!strcmp(property, "ToolTip")) {
        snprintf(text, sizeof text, "%s: %s", titles[item->which],
                 status_words[item->state]);
        return sd_bus_message_append(reply, "(sa(iiay)ss)", "", 0, text, "");
    }
    const char *value = text; /* the overlay and attention icons: none */
    if (!strcmp(property, "Category"))
        value = "ApplicationStatus";
    else if (!strcmp(property, "Id"))
        snprintf(text, sizeof text, "emacs-tray-%s", profile);
    else if (!strcmp(property, "Title"))
        value = titles[item->which];
    else if (!strcmp(property, "Status"))
        value = "Active";
    else if (!strcmp(property, "IconName"))
        icon_name(profile, item->shown, text, sizeof text);
    else if (!strcmp(property, "IconThemePath"))
        value = icon_dir;
    return sd_bus_message_append(reply, "s", value);
}

static int item_activate(sd_bus_message *message, void *userdata, sd_bus_error *error) {
    (void)error;
    Item *item = userdata;
    run_action("open", profiles[item->which]);
    return sd_bus_reply_method_return(message, NULL);
}

static int no_op(sd_bus_message *message, void *userdata, sd_bus_error *error) {
    (void)userdata, (void)error;
    return sd_bus_reply_method_return(message, NULL);
}

static const sd_bus_vtable item_vtable[] = {
    SD_BUS_VTABLE_START(0),
    SD_BUS_PROPERTY("Category", "s", item_property, 0, 0),
    SD_BUS_PROPERTY("Id", "s", item_property, 0, 0),
    SD_BUS_PROPERTY("Title", "s", item_property, 0, 0),
    SD_BUS_PROPERTY("Status", "s", item_property, 0, 0),
    SD_BUS_PROPERTY("WindowId", "i", item_property, 0, 0),
    SD_BUS_PROPERTY("IconThemePath", "s", item_property, 0, 0),
    SD_BUS_PROPERTY("IconName", "s", item_property, 0, 0),
    SD_BUS_PROPERTY("IconPixmap", "a(iiay)", item_property, 0, 0),
    SD_BUS_PROPERTY("OverlayIconName", "s", item_property, 0, 0),
    SD_BUS_PROPERTY("OverlayIconPixmap", "a(iiay)", item_property, 0, 0),
    SD_BUS_PROPERTY("AttentionIconName", "s", item_property, 0, 0),
    SD_BUS_PROPERTY("AttentionIconPixmap", "a(iiay)", item_property, 0, 0),
    SD_BUS_PROPERTY("AttentionMovieName", "s", item_property, 0, 0),
    SD_BUS_PROPERTY("ToolTip", "(sa(iiay)ss)", item_property, 0, 0),
    SD_BUS_PROPERTY("ItemIsMenu", "b", item_property, 0, 0),
    SD_BUS_PROPERTY("Menu", "o", item_property, 0, 0),
    SD_BUS_METHOD("Activate", "ii", "", item_activate, 0),
    SD_BUS_METHOD("SecondaryActivate", "ii", "", no_op, 0),
    SD_BUS_METHOD("ContextMenu", "ii", "", no_op, 0),
    SD_BUS_METHOD("Scroll", "is", "", no_op, 0),
    SD_BUS_SIGNAL("NewTitle", "", 0),
    SD_BUS_SIGNAL("NewIcon", "", 0),
    SD_BUS_SIGNAL("NewAttentionIcon", "", 0),
    SD_BUS_SIGNAL("NewOverlayIcon", "", 0),
    SD_BUS_SIGNAL("NewToolTip", "", 0),
    SD_BUS_SIGNAL("NewStatus", "s", 0),
    SD_BUS_VTABLE_END,
};

/* ---- com.canonical.dbusmenu: item 0 is the menu, 1..4 its entries ---- */

static int append_properties(sd_bus_message *reply, Item *item, int id) {
    int result = sd_bus_message_open_container(reply, 'a', "{sv}");
    if (result < 0) return result;
    if (id == 0)
        result = sd_bus_message_append(reply, "{sv}", "children-display", "s", "submenu");
    else
        result = sd_bus_message_append(reply, "{sv}{sv}{sv}", "label", "s", labels[id],
                                       "enabled", "b", item_enabled(id, item->state),
                                       "visible", "b", 1);
    if (result < 0) return result;
    return sd_bus_message_close_container(reply);
}

/* One (ia{sv}av) entry; the menu itself carries its entries as children. */
static int append_layout(sd_bus_message *reply, Item *item, int id, int children) {
    int result = sd_bus_message_open_container(reply, 'r', "ia{sv}av");
    if (result >= 0) result = sd_bus_message_append(reply, "i", id);
    if (result >= 0) result = append_properties(reply, item, id);
    if (result >= 0) result = sd_bus_message_open_container(reply, 'a', "v");
    for (int child = 1; result >= 0 && id == 0 && children && child < ITEM_COUNT;
         child++) {
        result = sd_bus_message_open_container(reply, 'v', "(ia{sv}av)");
        if (result >= 0) result = append_layout(reply, item, child, 0);
        if (result >= 0) result = sd_bus_message_close_container(reply);
    }
    if (result >= 0) result = sd_bus_message_close_container(reply);
    if (result >= 0) result = sd_bus_message_close_container(reply);
    return result;
}

static int menu_layout(sd_bus_message *message, void *userdata, sd_bus_error *error) {
    Item *item = userdata;
    int parent, depth;
    sd_bus_message *reply = NULL;
    int result = sd_bus_message_read(message, "ii", &parent, &depth);
    if (result < 0) return result;
    if (parent < 0 || parent >= ITEM_COUNT)
        return sd_bus_error_set(error, SD_BUS_ERROR_INVALID_ARGS, "No such menu item");
    result = sd_bus_message_new_method_return(message, &reply);
    if (result >= 0) result = sd_bus_message_append(reply, "u", item->revision);
    if (result >= 0) result = append_layout(reply, item, parent, depth != 0);
    if (result >= 0) result = sd_bus_send(NULL, reply, NULL);
    sd_bus_message_unref(reply);
    return result;
}

static int menu_group_properties(sd_bus_message *message, void *userdata,
                                 sd_bus_error *error) {
    (void)error;
    Item *item = userdata;
    const int32_t *ids;
    size_t size;
    sd_bus_message *reply = NULL;
    int result = sd_bus_message_read_array(message, 'i', (const void **)&ids, &size);
    if (result < 0) return result;
    size_t count = size / sizeof *ids;
    result = sd_bus_message_new_method_return(message, &reply);
    if (result >= 0) result = sd_bus_message_open_container(reply, 'a', "(ia{sv})");
    /* No ids asks for every item. */
    for (size_t i = 0; result >= 0 && i < (count ? count : ITEM_COUNT); i++) {
        int id = count ? ids[i] : (int)i;
        if (id < 0 || id >= ITEM_COUNT) continue;
        result = sd_bus_message_open_container(reply, 'r', "ia{sv}");
        if (result >= 0) result = sd_bus_message_append(reply, "i", id);
        if (result >= 0) result = append_properties(reply, item, id);
        if (result >= 0) result = sd_bus_message_close_container(reply);
    }
    if (result >= 0) result = sd_bus_message_close_container(reply);
    if (result >= 0) result = sd_bus_send(NULL, reply, NULL);
    sd_bus_message_unref(reply);
    return result;
}

static int menu_get_property(sd_bus_message *message, void *userdata,
                             sd_bus_error *error) {
    Item *item = userdata;
    int id;
    const char *name;
    int result = sd_bus_message_read(message, "is", &id, &name);
    if (result < 0) return result;
    if (id == 0 && !strcmp(name, "children-display"))
        return sd_bus_reply_method_return(message, "v", "s", "submenu");
    if (id >= 1 && id < ITEM_COUNT && !strcmp(name, "label"))
        return sd_bus_reply_method_return(message, "v", "s", labels[id]);
    if (id >= 1 && id < ITEM_COUNT && !strcmp(name, "enabled"))
        return sd_bus_reply_method_return(message, "v", "b",
                                          item_enabled(id, item->state));
    if (id >= 1 && id < ITEM_COUNT && !strcmp(name, "visible"))
        return sd_bus_reply_method_return(message, "v", "b", 1);
    return sd_bus_error_set(error, SD_BUS_ERROR_INVALID_ARGS, "No such property");
}

/* Reads one (id, event, data, timestamp) and runs a click on an enabled entry. */
static int handle_event(sd_bus_message *message, Item *item) {
    int id;
    const char *event;
    int result = sd_bus_message_read(message, "is", &id, &event);
    if (result >= 0) result = sd_bus_message_skip(message, "vu");
    if (result < 0) return result;
    if (!strcmp(event, "clicked") && id >= 1 && id < ITEM_COUNT &&
        item_enabled(id, unit_state(item)))
        run_action(actions[id], profiles[item->which]);
    return 0;
}

static int menu_event(sd_bus_message *message, void *userdata, sd_bus_error *error) {
    (void)error;
    int result = handle_event(message, userdata);
    return result < 0 ? result : sd_bus_reply_method_return(message, NULL);
}

static int menu_event_group(sd_bus_message *message, void *userdata,
                            sd_bus_error *error) {
    (void)error;
    int result = sd_bus_message_enter_container(message, 'a', "(isvu)");
    while (result >= 0 &&
           (result = sd_bus_message_enter_container(message, 'r', "isvu")) > 0) {
        result = handle_event(message, userdata);
        if (result >= 0) result = sd_bus_message_exit_container(message);
    }
    return result < 0 ? result : sd_bus_reply_method_return(message, "ai", 0);
}

/* The layout sent is always current: nothing to update before showing. */
static int menu_about_to_show(sd_bus_message *message, void *userdata,
                              sd_bus_error *error) {
    (void)userdata, (void)error;
    return sd_bus_reply_method_return(message, "b", 0);
}

static int menu_about_to_show_group(sd_bus_message *message, void *userdata,
                                    sd_bus_error *error) {
    (void)userdata, (void)error;
    return sd_bus_reply_method_return(message, "aiai", 0, 0);
}

static int menu_property(sd_bus *connection, const char *path, const char *interface,
                         const char *property, sd_bus_message *reply, void *userdata,
                         sd_bus_error *error) {
    (void)connection, (void)path, (void)interface, (void)userdata, (void)error;
    if (!strcmp(property, "Version")) return sd_bus_message_append(reply, "u", 3);
    if (!strcmp(property, "IconThemePath")) return sd_bus_message_append(reply, "as", 0);
    return sd_bus_message_append(
        reply, "s", !strcmp(property, "TextDirection") ? "ltr" : "normal");
}

static const sd_bus_vtable menu_vtable[] = {
    SD_BUS_VTABLE_START(0),
    SD_BUS_PROPERTY("Version", "u", menu_property, 0, 0),
    SD_BUS_PROPERTY("TextDirection", "s", menu_property, 0, 0),
    SD_BUS_PROPERTY("Status", "s", menu_property, 0, 0),
    SD_BUS_PROPERTY("IconThemePath", "as", menu_property, 0, 0),
    SD_BUS_METHOD("GetLayout", "iias", "u(ia{sv}av)", menu_layout, 0),
    SD_BUS_METHOD("GetGroupProperties", "aias", "a(ia{sv})", menu_group_properties, 0),
    SD_BUS_METHOD("GetProperty", "is", "v", menu_get_property, 0),
    SD_BUS_METHOD("Event", "isvu", "", menu_event, 0),
    SD_BUS_METHOD("EventGroup", "a(isvu)", "ai", menu_event_group, 0),
    SD_BUS_METHOD("AboutToShow", "i", "b", menu_about_to_show, 0),
    SD_BUS_METHOD("AboutToShowGroup", "ai", "aiai", menu_about_to_show_group, 0),
    SD_BUS_SIGNAL("ItemsPropertiesUpdated", "a(ia{sv})a(ias)", 0),
    SD_BUS_SIGNAL("LayoutUpdated", "ui", 0),
    SD_BUS_SIGNAL("ItemActivationRequested", "iu", 0),
    SD_BUS_VTABLE_END,
};

/* ---- Setup ---- */

static int add_item(int which) {
    Item *item = &items[which];
    char unit[64];
    item->which = which;
    item->state = item->shown = -1;
    snprintf(item->item_path, sizeof item->item_path, "/StatusNotifierItem/%s",
             profiles[which]);
    snprintf(item->menu_path, sizeof item->menu_path, "/MenuBar/%s", profiles[which]);
    snprintf(unit, sizeof unit, "emacs@%s.service", profiles[which]);
    int result =
        sd_bus_path_encode("/org/freedesktop/systemd1/unit", unit, &item->unit_path);
    if (result >= 0)
        result = sd_bus_add_object_vtable(bus, NULL, item->item_path, ITEM_INTERFACE,
                                          item_vtable, item);
    if (result >= 0)
        result = sd_bus_add_object_vtable(bus, NULL, item->menu_path, MENU_INTERFACE,
                                          menu_vtable, item);
    if (result >= 0)
        result = sd_bus_match_signal(bus, NULL, SYSTEMD, item->unit_path,
                                     "org.freedesktop.DBus.Properties",
                                     "PropertiesChanged", on_unit_changed, item);
    return result;
}

int main(void) {
    ssize_t length = readlink("/proc/self/exe", icon_dir, sizeof icon_dir - 1);
    char *slash = length > 0 ? strrchr(icon_dir, '/') : NULL;
    if (!slash) return 1;
    snprintf(slash, sizeof icon_dir - (size_t)(slash - icon_dir), "/icons");

    sd_event *event = NULL;
    int result = sd_event_default(&event);
    if (result >= 0)
        result = sd_event_add_time_relative(event, &timer, CLOCK_MONOTONIC, 0, 0, on_tick,
                                            NULL);
    if (result >= 0) result = sd_event_source_set_enabled(timer, SD_EVENT_OFF);
    if (result >= 0) result = sd_bus_open_user(&bus);
    /* systemd sends unit changes only to connections that subscribed. */
    if (result >= 0)
        result = sd_bus_call_method(bus, SYSTEMD, "/org/freedesktop/systemd1",
                                    SYSTEMD ".Manager", "Subscribe", NULL, NULL, NULL);
    for (int i = 0; result >= 0 && i < 2; i++) result = add_item(i);
    if (result >= 0)
        result = sd_bus_add_match(bus, NULL,
                                  "type='signal',sender='org.freedesktop.DBus',"
                                  "interface='org.freedesktop.DBus',"
                                  "member='NameOwnerChanged',arg0='" WATCHER "'",
                                  on_watcher_owner, NULL);
    if (result >= 0) result = sd_bus_attach_event(bus, event, 0);
    if (result < 0) {
        fprintf(stderr, "emacs-tray: %s\n", strerror(-result));
        return 1;
    }
    for (int i = 0; i < 2; i++) refresh(&items[i]);
    register_items();
    return sd_event_loop(event) < 0;
}
#endif /* EMACS_TRAY_TEST */
#endif /* __linux__ */
