/*
 * wellbeing: Android-style Digital Wellbeing for Windows and Linux, on top
 * of ActivityWatch. Daily screen time per app and website, nudges at daily
 * limits, focus mode and bedtime mode. Nothing is ever blocked.
 *
 * Usage comes from one ActivityWatch query a minute (~20 ms of aw-server's
 * time), made only while someone is at the keyboard. Settings live in
 * aw-server too (/api/0/settings: "wellbeing" for limits, the focus list,
 * bedtime and commands; "wellbeing.state" for focus mode and a paused
 * bedtime), edited from the dashboard (index.html, served by aw-server at
 * /pages/wellbeing/) and read every 10 s.
 *
 *  - Limits: a nudge 5 minutes before, at the limit, and every 30 minutes
 *    over it.
 *  - Focus mode: Do Not Disturb, and a nudge when a listed app or site comes
 *    to the front.
 *  - Bedtime: grayscale screen and Do Not Disturb between two times.
 *
 * Commands:
 *   wellbeing            run (one instance; started at login)
 *   wellbeing --status   JSON for a bar widget: today's total and top app
 *   wellbeing --focus    toggle focus mode
 *   wellbeing --bedtime  pause tonight's bedtime, or resume it
 *
 * The platform parts are platform-windows.h and platform-linux.h: nudges,
 * the app in front, idle time, grayscale and the event loop.
 *
 * Build: make (Linux), or pwsh -File ..\..\..\windots\yasb\Build-Native.ps1
 *        wellbeing.c -Libs ws2_32,dwmapi -Windows (windots setup does this).
 * Tests: make test, or wellbeing.test.c with gcc on Windows.
 */
#ifdef _WIN32
#define _WIN32_WINNT 0x0A00
#else
#define _GNU_SOURCE
#endif
#include "../lib/http.h"
#include "../lib/json.h"
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#define USAGE_EVERY_S 60
#define SETTINGS_EVERY_S 10
/* Where the platform can't report focus changes, how often to look. */
#define FOCUS_POLL_S 3
/* Idle this long and usage stops growing, so the query is skipped. */
#define IDLE_S 180
#define WARN_BEFORE_S 300
#define REMIND_EVERY_S 1800
/* A focus-mode nudge for the same app or site at most this often. */
#define FOCUS_REPEAT_S 600
#define MAX_RULES 64
#define MAX_USAGE 200
#define NAME_SIZE 128
#define COMMAND_SIZE 512

typedef enum { KIND_APP, KIND_SITE } Kind;

typedef struct {
    Kind kind;
    char name[NAME_SIZE];
    int minutes;
    /* Usage in seconds at which the next nudge is due; < 0: not yet known,
       set without nudging so a restart doesn't repeat old nudges. */
    double next_s;
} Limit;

typedef struct {
    Kind kind;
    char name[NAME_SIZE];
    double last_nudge; /* monotonic seconds; 0: never */
} FocusRule;

typedef struct {
    Limit limits[MAX_RULES];
    int limit_count;
    FocusRule focus[MAX_RULES];
    int focus_count;
    /* Minutes after midnight; -1 when bedtime is off. */
    int bedtime_start, bedtime_end;
    /* Shell commands; empty: the platform's default. */
    char dnd_on[COMMAND_SIZE], dnd_off[COMMAND_SIZE], grayscale_on[COMMAND_SIZE], grayscale_off[COMMAND_SIZE];
} Config;

typedef struct {
    int focus_mode;
    /* The date ("2026-09-28") of the scheduled bedtime that was paused, or "". */
    char bedtime_paused[16];
    /* Bedtime turned on by hand lasts until this time (epoch seconds); 0: not. */
    long long bedtime_until;
} State;

typedef struct {
    char name[NAME_SIZE];
    double seconds;
} Entry;

typedef struct {
    int valid;
    double total;
    Entry apps[MAX_USAGE], sites[MAX_USAGE];
    int app_count, site_count;
} Usage;

typedef enum { NUDGE_NONE, NUDGE_WARNING, NUDGE_REACHED, NUDGE_OVER } Nudge;

/* ---- Text ---- */

static char lower(char c) {
    return c >= 'A' && c <= 'Z' ? (char)(c + 32) : c;
}

/* Case-insensitive, ASCII letters only: app names and domains. */
static int ci_equal(const char *a, const char *b) {
    for (; *a && *b; a++, b++)
        if (lower(*a) != lower(*b)) return 0;
    return *a == *b;
}

/* A copy of text's token i (a value inside a larger JSON document). */
static char *token_text(const Json *json, int i) {
    if (i < 0) return NULL;
    size_t length = (size_t)(json->tokens[i].end - json->tokens[i].start);
    char *copy = malloc(length + 1);
    memcpy(copy, json->text + json->tokens[i].start, length);
    copy[length] = 0;
    return copy;
}

static void format_duration(double seconds, char *out, size_t size) {
    long minutes = (long)(seconds / 60);
    if (minutes >= 60) snprintf(out, size, "%ldh %ldm", minutes / 60, minutes % 60);
    else snprintf(out, size, "%ldm", minutes);
}

/* "23:30" as minutes after midnight, or -1. */
static int parse_time(const char *text) {
    int hours, minutes;
    if (sscanf(text, "%d:%d", &hours, &minutes) != 2 || hours < 0 || hours > 23 || minutes < 0 || minutes > 59) return -1;
    return hours * 60 + minutes;
}

/* An app without Windows' ".exe"; a site as it is. */
static void display_name(Kind kind, const char *name, char *out, size_t size) {
    size_t length = strlen(name);
    int exe = kind == KIND_APP && length > 4 && ci_equal(name + length - 4, ".exe");
    snprintf(out, size, "%.*s", (int)(exe ? length - 4 : length), name);
}

/* ---- Settings ---- */

static Kind kind_of(const Json *json, int rule) {
    char kind[16];
    json_string(json, json_get(json, rule, "kind"), kind, sizeof kind);
    return strcmp(kind, "app") == 0 ? KIND_APP : KIND_SITE;
}

/* The "wellbeing" setting's JSON into a config. Limits keep their nudge
   progress, and focus rules their last nudge, from previous (the config
   before a reload) when unchanged. Anything missing or null: off/empty.

   {"limits": [{"kind": "site", "name": "youtube.com", "minutes": 30}],
    "focus": [{"kind": "app", "name": "Discord.exe"}],
    "bedtime": {"enabled": true, "start": "23:30", "end": "07:00"},
    "commands": {"dnd_on": "", "dnd_off": "", "grayscale_on": "", "grayscale_off": ""}} */
static void parse_config(const char *text, Config *out, const Config *previous) {
    memset(out, 0, sizeof *out);
    out->bedtime_start = out->bedtime_end = -1;
    Json json;
    if (!text || !json_parse(text, &json)) return;
    int limits = json_get(&json, 0, "limits");
    for (int i = 0; i < json_size(&json, limits) && out->limit_count < MAX_RULES; i++) {
        int rule = json_at(&json, limits, i);
        Limit *limit = &out->limits[out->limit_count];
        limit->kind = kind_of(&json, rule);
        json_string(&json, json_get(&json, rule, "name"), limit->name, sizeof limit->name);
        limit->minutes = (int)json_number(&json, json_get(&json, rule, "minutes"), 0);
        limit->next_s = -1;
        if (!*limit->name || limit->minutes <= 0) continue;
        for (int p = 0; previous && p < previous->limit_count; p++) {
            const Limit *old = &previous->limits[p];
            if (old->kind == limit->kind && old->minutes == limit->minutes && ci_equal(old->name, limit->name)) limit->next_s = old->next_s;
        }
        out->limit_count++;
    }
    int focus = json_get(&json, 0, "focus");
    for (int i = 0; i < json_size(&json, focus) && out->focus_count < MAX_RULES; i++) {
        int rule = json_at(&json, focus, i);
        FocusRule *f = &out->focus[out->focus_count];
        f->kind = kind_of(&json, rule);
        json_string(&json, json_get(&json, rule, "name"), f->name, sizeof f->name);
        if (!*f->name) continue;
        for (int p = 0; previous && p < previous->focus_count; p++)
            if (previous->focus[p].kind == f->kind && ci_equal(previous->focus[p].name, f->name)) f->last_nudge = previous->focus[p].last_nudge;
        out->focus_count++;
    }
    int bedtime = json_get(&json, 0, "bedtime");
    if (json_bool(&json, json_get(&json, bedtime, "enabled"))) {
        char time[16];
        json_string(&json, json_get(&json, bedtime, "start"), time, sizeof time);
        out->bedtime_start = parse_time(time);
        json_string(&json, json_get(&json, bedtime, "end"), time, sizeof time);
        out->bedtime_end = parse_time(time);
    }
    if (out->bedtime_start < 0 || out->bedtime_end < 0 || out->bedtime_start == out->bedtime_end) out->bedtime_start = out->bedtime_end = -1;
    int commands = json_get(&json, 0, "commands");
    json_string(&json, json_get(&json, commands, "dnd_on"), out->dnd_on, sizeof out->dnd_on);
    json_string(&json, json_get(&json, commands, "dnd_off"), out->dnd_off, sizeof out->dnd_off);
    json_string(&json, json_get(&json, commands, "grayscale_on"), out->grayscale_on, sizeof out->grayscale_on);
    json_string(&json, json_get(&json, commands, "grayscale_off"), out->grayscale_off, sizeof out->grayscale_off);
    json_free(&json);
}

/* The "wellbeing.state" setting:
   {"focus_mode": true, "bedtime_paused": "2026-09-28", "bedtime_until": 1790560800}. */
static void parse_state(const char *text, State *out) {
    memset(out, 0, sizeof *out);
    Json json;
    if (!text || !json_parse(text, &json)) return;
    out->focus_mode = json_bool(&json, json_get(&json, 0, "focus_mode"));
    json_string(&json, json_get(&json, 0, "bedtime_paused"), out->bedtime_paused, sizeof out->bedtime_paused);
    out->bedtime_until = (long long)json_number(&json, json_get(&json, 0, "bedtime_until"), 0);
    json_free(&json);
}

static void state_json(const State *state, char *out, size_t size) {
    snprintf(out, size, "{\"focus_mode\": %s, \"bedtime_paused\": \"%s\", \"bedtime_until\": %lld}", state->focus_mode ? "true" : "false",
        state->bedtime_paused, state->bedtime_until);
}

/* All settings ({"wellbeing": {...}, "wellbeing.state": {...}, ...}) into
   the config and state. Returns 0 if text isn't a settings object. */
static int parse_settings(const char *text, Config *config, const Config *previous, State *state) {
    Json json;
    if (!json_parse(text, &json) || json.tokens[0].type != JSMN_OBJECT) {
        json_free(&json);
        return 0;
    }
    char *part = token_text(&json, json_get(&json, 0, "wellbeing"));
    parse_config(part, config, previous);
    free(part);
    part = token_text(&json, json_get(&json, 0, "wellbeing.state"));
    parse_state(part, state);
    free(part);
    json_free(&json);
    return 1;
}

/* ---- Matching ---- */

/* A domain matches a rule for itself or any subdomain of it. */
static int domain_matches(const char *domain, const char *rule) {
    size_t length = strlen(domain), rule_length = strlen(rule);
    if (!rule_length || length < rule_length || !ci_equal(domain + length - rule_length, rule)) return 0;
    return length == rule_length || domain[length - rule_length - 1] == '.';
}

static int rule_matches(Kind kind, const char *name, const char *app, const char *domain) {
    return kind == KIND_APP ? ci_equal(app, name) : domain && *domain && domain_matches(domain, name);
}

/* Today's seconds for a rule. */
static double rule_usage(const Usage *u, Kind kind, const char *name) {
    double seconds = 0;
    if (kind == KIND_APP) {
        for (int i = 0; i < u->app_count; i++)
            if (ci_equal(u->apps[i].name, name)) seconds += u->apps[i].seconds;
    }
    else
        for (int i = 0; i < u->site_count; i++)
            if (domain_matches(u->sites[i].name, name)) seconds += u->sites[i].seconds;
    return seconds;
}

/* Browser names as the window watchers record them: Windows exes, and Linux
   window classes. The web extension's time counts only while one of these
   is in front. */
static const char *browsers[] = {
    "msedge.exe", "chrome.exe", "brave.exe", "firefox.exe", "vivaldi.exe", "opera.exe",
    "firefox", "firefox-esr", "librewolf", "zen", "zen-browser", "chromium", "chromium-browser",
    "google-chrome", "Google-chrome", "brave-browser", "Brave-browser", "microsoft-edge", "Microsoft-edge",
    "vivaldi-stable", "Vivaldi-stable", "opera"};

static int is_browser(const char *app) {
    for (size_t i = 0; i < sizeof browsers / sizeof *browsers; i++)
        if (ci_equal(app, browsers[i])) return 1;
    return 0;
}

/* The host of a URL, without "www.". */
static void domain_of(const char *url, char *out, size_t size) {
    const char *host = strstr(url, "://");
    host = host ? host + 3 : url;
    if (strncmp(host, "www.", 4) == 0) host += 4;
    size_t length = strcspn(host, "/:?#");
    snprintf(out, size, "%.*s", (int)(length < size ? length : size - 1), host);
}

/* ---- Limits ---- */

/* The first nudge point past used: 5 minutes before the limit (for limits
   of 10 minutes or more), the limit, then every 30 minutes over. */
static double threshold_after(double limit_s, double used_s) {
    if (limit_s >= 2 * WARN_BEFORE_S && used_s < limit_s - WARN_BEFORE_S) return limit_s - WARN_BEFORE_S;
    if (used_s < limit_s) return limit_s;
    return limit_s + REMIND_EVERY_S * (floor((used_s - limit_s) / REMIND_EVERY_S) + 1);
}

/* Whether this much use calls for a nudge, and which. */
static Nudge check_limit(Limit *limit, double used_s) {
    double limit_s = limit->minutes * 60.0;
    int known = limit->next_s >= 0;
    int due = known && used_s >= limit->next_s;
    limit->next_s = known && !due ? limit->next_s : threshold_after(limit_s, used_s);
    if (!due) return NUDGE_NONE;
    return used_s < limit_s ? NUDGE_WARNING : used_s < limit_s + REMIND_EVERY_S ? NUDGE_REACHED : NUDGE_OVER;
}

static void nudge_text(const Limit *limit, Nudge nudge, double used_s, char *title, size_t title_size, char *body, size_t body_size) {
    char name[NAME_SIZE], used[32], allowed[32], over[32];
    display_name(limit->kind, limit->name, name, sizeof name);
    format_duration(used_s, used, sizeof used);
    format_duration(limit->minutes * 60.0, allowed, sizeof allowed);
    format_duration(used_s - limit->minutes * 60.0, over, sizeof over);
    if (nudge == NUDGE_WARNING) {
        snprintf(title, title_size, "%s: 5 minutes left", name);
        snprintf(body, body_size, "%s of your %s daily limit used.", used, allowed);
    }
    else if (nudge == NUDGE_REACHED) {
        snprintf(title, title_size, "%s: daily limit reached", name);
        snprintf(body, body_size, "%s today. Time for a break?", used);
    }
    else {
        snprintf(title, title_size, "%s: %s over your limit", name, over);
        snprintf(body, body_size, "%s today, of %s.", used, allowed);
    }
}

/* ---- Usage ---- */

static int parse_entries(const Json *json, int array, const char *key, Entry *entries) {
    int count = 0;
    for (int i = 0; i < json_size(json, array) && count < MAX_USAGE; i++) {
        int event = json_at(json, array, i);
        int name = json_get(json, json_get(json, event, "data"), key);
        if (name < 0) continue;
        entries[count].seconds = json_number(json, json_get(json, event, "duration"), 0);
        json_string(json, name, entries[count].name, sizeof entries[count].name);
        count++;
    }
    return count;
}

/* The query's result: [{"apps": [events], "sites": [events], "total": s}]. */
static int parse_usage(const char *text, Usage *out) {
    memset(out, 0, sizeof *out);
    Json json;
    if (!json_parse(text, &json)) return 0;
    int result = json_at(&json, 0, 0);
    int total = json_get(&json, result, "total");
    if (total >= 0) {
        out->total = json_number(&json, total, 0);
        out->app_count = parse_entries(&json, json_get(&json, result, "apps"), "app", out->apps);
        out->site_count = parse_entries(&json, json_get(&json, result, "sites"), "$domain", out->sites);
        out->valid = 1;
    }
    json_free(&json);
    return out->valid;
}

/* Today, local midnight to midnight, as UTC ISO 8601. */
static void today_period(time_t now, char *out, size_t size) {
    struct tm local = *localtime(&now);
    local.tm_hour = local.tm_min = local.tm_sec = 0;
    local.tm_isdst = -1;
    time_t start = mktime(&local);
    local.tm_mday++;
    local.tm_isdst = -1;
    time_t end = mktime(&local);
    char a[32], b[32];
    strftime(a, sizeof a, "%Y-%m-%dT%H:%M:%SZ", gmtime(&start));
    strftime(b, sizeof b, "%Y-%m-%dT%H:%M:%SZ", gmtime(&end));
    snprintf(out, size, "%s/%s", a, b);
}

/* The query for today's time per app (and per domain with the web
   extension), idle time left out. */
static void usage_query(const char *period, int with_web, char *out, size_t size) {
    char browser_list[1024] = "";
    for (size_t i = 0; i < sizeof browsers / sizeof *browsers; i++)
        snprintf(browser_list + strlen(browser_list), sizeof browser_list - strlen(browser_list), "%s'%s'", i ? ", " : "", browsers[i]);
    snprintf(out, size,
        "{\"timeperiods\": [\"%s\"], \"query\": ["
        "\"afk = flood(query_bucket(find_bucket('aw-watcher-afk_')));\","
        "\"windows = flood(query_bucket(find_bucket('aw-watcher-window_')));\","
        "\"windows = filter_period_intersect(windows, filter_keyvals(afk, 'status', ['not-afk']));\","
        "%s%s%s"
        "\"RETURN = {'total': sum_durations(windows), 'apps': sort_by_duration(merge_events_by_keys(windows, ['app']))%s};\"]}",
        period,
        with_web ? "\"browsers = filter_keyvals(windows, 'app', [" : "", with_web ? browser_list : "",
        with_web ? "]);\", \"web = split_url_events(filter_period_intersect(flood(query_bucket(find_bucket('aw-watcher-web-'))), browsers));\"," : "",
        with_web ? ", 'sites': sort_by_duration(merge_events_by_keys(web, ['$domain']))" : "");
}

/* ---- Bedtime ---- */

static int in_bedtime(int minute_of_day, int start, int end) {
    if (start < 0) return 0;
    return start < end ? minute_of_day >= start && minute_of_day < end : minute_of_day >= start || minute_of_day < end;
}

/* The date a bedtime started on: yesterday's for the part after midnight. */
static void bedtime_date(time_t now, int start, char *out, size_t size) {
    struct tm local = *localtime(&now);
    if (local.tm_hour * 60 + local.tm_min < start) now -= 86400;
    strftime(out, size, "%Y-%m-%d", localtime(&now));
}

/* Bedtime turned on by hand without a schedule lasts until this time. */
#define DEFAULT_BEDTIME_END (7 * 60)

/* The first time after now that a day reaches this minute after midnight. */
static time_t next_time_of_day(time_t now, int minute) {
    struct tm local = *localtime(&now);
    local.tm_hour = minute / 60, local.tm_min = minute % 60, local.tm_sec = 0, local.tm_isdst = -1;
    time_t at = mktime(&local);
    if (at <= now) {
        local.tm_mday++, local.tm_isdst = -1;
        at = mktime(&local);
    }
    return at;
}

/* Whether bedtime is on: turned on by hand and not over, or scheduled now
   and not paused. */
static int bedtime_active(const State *s, const Config *c, time_t now) {
    if (s->bedtime_until > (long long)now) return 1;
    struct tm local = *localtime(&now);
    if (!in_bedtime(local.tm_hour * 60 + local.tm_min, c->bedtime_start, c->bedtime_end)) return 0;
    char date[16];
    bedtime_date(now, c->bedtime_start, date, sizeof date);
    return strcmp(date, s->bedtime_paused) != 0;
}

/* When the bedtime that's on ends, in minutes after midnight. */
static int bedtime_end_minute(const State *s, const Config *c, time_t now) {
    if (s->bedtime_until > (long long)now) {
        time_t until = (time_t)s->bedtime_until;
        struct tm local = *localtime(&until);
        return local.tm_hour * 60 + local.tm_min;
    }
    return c->bedtime_end >= 0 ? c->bedtime_end : DEFAULT_BEDTIME_END;
}

typedef enum { BEDTIME_STARTED, BEDTIME_RESUMED, BEDTIME_ENDED, BEDTIME_PAUSED } BedtimeChange;

/* The bedtime key (--bedtime), as on Android. Off: on now, until the
   schedule's end time (or 07:00), or a paused scheduled bedtime resumes.
   On: one turned on by hand ends; a scheduled one pauses until the next
   night. */
static BedtimeChange toggle_bedtime(State *s, const Config *c, time_t now) {
    struct tm local = *localtime(&now);
    int scheduled = in_bedtime(local.tm_hour * 60 + local.tm_min, c->bedtime_start, c->bedtime_end);
    char date[16] = "";
    if (scheduled) bedtime_date(now, c->bedtime_start, date, sizeof date);
    if (bedtime_active(s, c, now)) {
        s->bedtime_until = 0;
        if (!scheduled) return BEDTIME_ENDED;
        snprintf(s->bedtime_paused, sizeof s->bedtime_paused, "%s", date);
        return BEDTIME_PAUSED;
    }
    if (scheduled) {
        s->bedtime_paused[0] = 0;
        return BEDTIME_RESUMED;
    }
    s->bedtime_until = next_time_of_day(now, c->bedtime_end >= 0 ? c->bedtime_end : DEFAULT_BEDTIME_END);
    return BEDTIME_STARTED;
}

/* ---- Status for bars ---- */

/* The widget's JSON; top_placeholder stands in for the top app when there's
   no usage to show. The icon is the platform's (PLATFORM_ICON_*). */
static void status_json(const Usage *u, int focus, const char *icon, const char *top_placeholder, char *out, size_t size) {
    char total[32] = "--", top[NAME_SIZE + 32], escaped[2 * (NAME_SIZE + 32)];
    snprintf(top, sizeof top, "%s", top_placeholder);
    if (u->valid) {
        format_duration(u->total, total, sizeof total);
        if (u->app_count) {
            char name[NAME_SIZE], time[32];
            display_name(KIND_APP, u->apps[0].name, name, sizeof name);
            format_duration(u->apps[0].seconds, time, sizeof time);
            snprintf(top, sizeof top, "%s %s", name, time);
        }
    }
    json_escape(top, escaped, sizeof escaped);
    snprintf(out, size, "{\"icon\": \"%s\", \"total\": \"%s\", \"top\": \"%s\", \"focus\": \"%s\"}", icon, total, escaped, focus ? "on" : "off");
}

/* ---- The running helper ---- */

#ifndef WELLBEING_TEST

/* The platform provides (see platform-*.h):
     double platform_seconds(void);           monotonic
     double platform_idle_seconds(void);      < 0: unknown
     int platform_foreground(char *app, size_t size);  0: unknown here
     void platform_notify(const char *title, const char *body, double progress, int urgent);
     int platform_grayscale(int on);          0: not built in
     void platform_default_commands(Config *config);
     void platform_run(const char *command);
     void platform_watch_focus(int on);       report focus changes to core_check_focus
     void platform_data_path(const char *name, char *out, size_t size);
     void platform_write_stdout(const char *text);
     int platform_running(void);
     void platform_send(Command command);    to the running helper, which
                                             calls core_toggle_focus/_bedtime
     void platform_sync_soon(void);           core_settings shortly, not now
     int platform_daemon(void);               the event loop
     PLATFORM_ICON_TIMER, PLATFORM_ICON_FOCUS */
typedef enum { COMMAND_FOCUS, COMMAND_BEDTIME } Command;

static void core_toggle_focus(void);
static void core_toggle_bedtime(void);

#ifdef _WIN32
#include "platform-windows.h"
#else
#include "platform-linux.h"
#endif

static Config config;
static State state;
static Usage usage;
static int settings_known, dnd_applied = -1, grayscale_applied;
/* A toggle not yet saved to aw-server (unreachable at the time). */
static int state_unsaved;
static int today = -1;
static char window_bucket[NAME_SIZE], afk_bucket[NAME_SIZE], web_bucket[NAME_SIZE];

static void write_file(const char *path, const char *text) {
    FILE *file = fopen(path, "wb");
    if (!file) return;
    fputs(text, file);
    fclose(file);
}

static char *read_file(const char *path) {
    FILE *file = fopen(path, "rb");
    if (!file) return NULL;
    fseek(file, 0, SEEK_END);
    long size = ftell(file);
    fseek(file, 0, SEEK_SET);
    char *text = calloc((size_t)size + 1, 1);
    if (fread(text, 1, (size_t)size, file) != (size_t)size) text[0] = 0;
    fclose(file);
    return text;
}

/* Each nudge also goes to wellbeing.log, to see what was nudged and when. */
static void notify(const char *title, const char *body, double progress) {
    char path[512], line[768], stamp[32];
    time_t now = time(NULL);
    strftime(stamp, sizeof stamp, "%Y-%m-%d %H:%M:%S", localtime(&now));
    platform_data_path("wellbeing.log", path, sizeof path);
    FILE *log = fopen(path, "ab");
    if (log) {
        snprintf(line, sizeof line, "%s  %s: %s\n", stamp, title, body);
        fputs(line, log);
        fclose(log);
    }
    /* Urgent while Do Not Disturb is on, which would hide it otherwise. */
    platform_notify(title, body, progress, dnd_applied == 1);
}

static void write_status(void) {
    char path[512], json[512];
    platform_data_path("wellbeing-status.json", path, sizeof path);
    status_json(&usage, state.focus_mode, state.focus_mode ? PLATFORM_ICON_FOCUS : PLATFORM_ICON_TIMER, "no activity yet", json, sizeof json);
    write_file(path, json);
}

static int save_state(void) {
    char json[128];
    state_json(&state, json, sizeof json);
    return http_request("POST", "/api/0/settings/wellbeing.state", json, NULL, 0) == 200;
}

static void run_or_builtin(const char *command, int grayscale_on) {
    if (*command) platform_run(command);
    else if (grayscale_on >= 0) platform_grayscale(grayscale_on);
}

/* Do Not Disturb follows focus mode and bedtime; only changes are applied,
   so turning it on by hand outside them is left alone. */
static void sync_dnd(int bedtime_on) {
    int wanted = state.focus_mode || bedtime_on;
    if (wanted == dnd_applied || (dnd_applied < 0 && !wanted)) {
        dnd_applied = wanted;
        return;
    }
    dnd_applied = wanted;
    Config defaults = config;
    platform_default_commands(&defaults);
    run_or_builtin(wanted ? defaults.dnd_on : defaults.dnd_off, -1);
}

static void update_bedtime(void) {
    time_t now = time(NULL);
    int on = bedtime_active(&state, &config, now), started = on && !grayscale_applied;
    if (on != grayscale_applied) {
        grayscale_applied = on;
        Config defaults = config;
        platform_default_commands(&defaults);
        run_or_builtin(on ? defaults.grayscale_on : defaults.grayscale_off, on);
    }
    /* Do Not Disturb first, so the notice below goes through it. */
    sync_dnd(on);
    if (!started) return;
    char body[96];
    int end = bedtime_end_minute(&state, &config, now);
    snprintf(body, sizeof body, "Grayscale and Do Not Disturb until %02d:%02d.", end / 60, end % 60);
    notify("Bedtime", body, -1);
}

/* ActivityWatch's bucket ids, found once: they end in the hostname. */
static void find_buckets(void) {
    if (*window_bucket) return;
    static char response[1 << 16];
    if (http_request("GET", "/api/0/buckets/", NULL, response, sizeof response) != 200) return;
    Json json;
    if (!json_parse(response, &json)) return;
    for (int i = 0, at = 1; i < json_size(&json, 0); i++, at = json_skip(&json, at + 1)) {
        char id[NAME_SIZE];
        json_string(&json, at, id, sizeof id);
        if (!*window_bucket && strncmp(id, "aw-watcher-window", 17) == 0) snprintf(window_bucket, sizeof window_bucket, "%s", id);
        if (!*afk_bucket && strncmp(id, "aw-watcher-afk", 14) == 0) snprintf(afk_bucket, sizeof afk_bucket, "%s", id);
        if (!*web_bucket && strncmp(id, "aw-watcher-web", 14) == 0) snprintf(web_bucket, sizeof web_bucket, "%s", id);
    }
    json_free(&json);
}

/* The latest event's data field, from a bucket. */
static int latest(const char *bucket, const char *key, char *out, size_t size) {
    static char response[1 << 14];
    char path[256];
    *out = 0;
    if (!*bucket) return 0;
    snprintf(path, sizeof path, "/api/0/buckets/%s/events?limit=1", bucket);
    if (http_request("GET", path, NULL, response, sizeof response) != 200) return 0;
    Json json;
    if (!json_parse(response, &json)) return 0;
    json_string(&json, json_get(&json, json_get(&json, json_at(&json, 0, 0), "data"), key), out, size);
    json_free(&json);
    return *out != 0;
}

static int idle(void) {
    double seconds = platform_idle_seconds();
    if (seconds >= 0) return seconds >= IDLE_S;
    /* No idle time from the platform: ask the afk watcher. */
    char status[16];
    find_buckets();
    return latest(afk_bucket, "status", status, sizeof status) && strcmp(status, "afk") == 0;
}

static void core_check_focus(void);

/* The new state, and what follows from it: focus watching, grayscale,
   Do Not Disturb, and a notice when focus mode changes. */
static void set_state(const State *next) {
    int changed = settings_known && next->focus_mode != state.focus_mode;
    state = *next;
    if (changed || !settings_known) platform_watch_focus(state.focus_mode);
    settings_known = 1;
    /* Grayscale, and Do Not Disturb for both bedtime and focus mode. */
    update_bedtime();
    if (!changed) return;
    char body[256] = "Do Not Disturb is on.";
    if (state.focus_mode && config.focus_count) {
        snprintf(body, sizeof body, "Nudges for: ");
        for (int i = 0; i < config.focus_count; i++) {
            char name[NAME_SIZE];
            display_name(config.focus[i].kind, config.focus[i].name, name, sizeof name);
            snprintf(body + strlen(body), sizeof body - strlen(body), "%s%s", i ? ", " : "", name);
        }
    }
    notify(state.focus_mode ? "Focus mode on" : "Focus mode off", state.focus_mode ? body : "Welcome back.", -1);
    write_status();
    /* What's in front is checked by the platform's focus watching, started
       above: not here, where a slow aw-server would hold up the notice. */
}

/* Settings every 10 s: a change from the dashboard (or another helper's
   toggle) takes effect here. */
static void core_settings(void) {
    static char response[1 << 16];
    /* A toggle made while aw-server was unreachable goes up first, or the
       old state there would undo it. */
    if (state_unsaved) {
        if (!save_state()) return;
        state_unsaved = 0;
    }
    if (http_request("GET", "/api/0/settings", NULL, response, sizeof response) != 200) return;
    Config fresh;
    State next;
    if (!parse_settings(response, &fresh, &config, &next)) return;
    config = fresh;
    set_state(&next);
}

/* --focus (the bar's middle-click): at once, and without aw-server if need
   be; saved there when it's reachable. */
static void core_toggle_focus(void) {
    State next = state;
    next.focus_mode = !next.focus_mode;
    settings_known = 1;
    set_state(&next);
    /* Saved by the settings sync just after, so a slow or missing aw-server
       never holds up the notice. */
    state_unsaved = 1;
    platform_sync_soon();
}

/* --bedtime: bedtime on now, or off (see toggle_bedtime). */
static void core_toggle_bedtime(void) {
    BedtimeChange change = toggle_bedtime(&state, &config, time(NULL));
    /* Turning it on brings its own notice ("Bedtime ... until"). */
    update_bedtime();
    if (change == BEDTIME_PAUSED) notify("Bedtime", "Paused until tomorrow's bedtime.", -1);
    else if (change == BEDTIME_ENDED) notify("Bedtime", "Off.", -1);
    /* Saved by the settings sync just after, so a slow or missing aw-server
       never holds up the notice. */
    state_unsaved = 1;
    platform_sync_soon();
}

/* Usage every minute, while someone is at the keyboard. */
static void core_usage(void) {
    update_bedtime();
    time_t now = time(NULL);
    struct tm local = *localtime(&now);
    if (local.tm_yday != today) {
        /* A new day: usage starts again from 0. */
        for (int i = 0; i < config.limit_count; i++) config.limits[i].next_s = -1;
        today = local.tm_yday;
        usage.valid = 0;
    }
    if (usage.valid && idle()) return;
    static char body[4096], response[1 << 17];
    char period[64];
    today_period(now, period, sizeof period);
    Usage fresh = {0};
    for (int with_web = 1; with_web >= 0 && !fresh.valid; with_web--) {
        usage_query(period, with_web, body, sizeof body);
        if (http_request("POST", "/api/0/query/", body, response, sizeof response) == 200) parse_usage(response, &fresh);
    }
    if (!fresh.valid) return;
    usage = fresh;
    for (int i = 0; i < config.limit_count; i++) {
        Limit *limit = &config.limits[i];
        double used = rule_usage(&usage, limit->kind, limit->name);
        Nudge nudge = check_limit(limit, used);
        if (nudge == NUDGE_NONE) continue;
        char title[160], text[256];
        nudge_text(limit, nudge, used, title, sizeof title, text, sizeof text);
        notify(title, text, used / (limit->minutes * 60.0));
    }
    write_status();
}

/* In focus mode: a nudge if what's in front is on the list. The platform
   calls this on focus changes, or every FOCUS_POLL_S where it can't tell. */
static void core_check_focus(void) {
    if (!state.focus_mode || !config.focus_count) return;
    char app[NAME_SIZE] = "", domain[256] = "", url[1024];
    find_buckets();
    if (!platform_foreground(app, sizeof app)) latest(window_bucket, "app", app, sizeof app);
    if (is_browser(app) && latest(web_bucket, "url", url, sizeof url)) domain_of(url, domain, sizeof domain);
    for (int i = 0; i < config.focus_count; i++) {
        FocusRule *rule = &config.focus[i];
        if (!rule_matches(rule->kind, rule->name, app, domain)) continue;
        double now = platform_seconds();
        if (rule->last_nudge && now - rule->last_nudge < FOCUS_REPEAT_S) return;
        rule->last_nudge = now;
        char name[NAME_SIZE], title[160];
        display_name(rule->kind, rule->name, name, sizeof name);
        snprintf(title, sizeof title, "Focus mode: %s", name);
        notify(title, "You wanted to stay away from this for now.", -1);
        return;
    }
}

static void core_start(void) {
    core_settings();
    core_usage();
}

/* On exit: grayscale off, so it doesn't outlive bedtime. */
static void core_stop(void) {
    if (!grayscale_applied) return;
    Config defaults = config;
    platform_default_commands(&defaults);
    run_or_builtin(defaults.grayscale_off, 0);
}

/* ---- Commands ---- */

static int print_status(void) {
    char path[512], fallback[256];
    platform_data_path("wellbeing-status.json", path, sizeof path);
    char *text = platform_running() ? read_file(path) : NULL;
    if (!text) {
        Usage none = {0};
        status_json(&none, 0, PLATFORM_ICON_TIMER, "wellbeing is not running", fallback, sizeof fallback);
    }
    platform_write_stdout(text ? text : fallback);
    free(text);
    return 0;
}

/* --focus and --bedtime: the running helper does it (at once, and with a
   notice). Without one, the state in aw-server changes for the next start. */
static int toggle(int focus) {
    if (platform_running()) {
        platform_send(focus ? COMMAND_FOCUS : COMMAND_BEDTIME);
        return 0;
    }
    static char response[1 << 16];
    if (http_request("GET", "/api/0/settings", NULL, response, sizeof response) != 200) {
        fputs("wellbeing: ActivityWatch (aw-server) is not reachable\n", stderr);
        return 1;
    }
    Config current;
    parse_settings(response, &current, NULL, &state);
    if (focus) state.focus_mode = !state.focus_mode;
    else toggle_bedtime(&state, &current, time(NULL));
    return save_state() ? 0 : 1;
}

static int wellbeing_main(int argc, char **argv) {
    const char *command = argc > 1 ? argv[1] : "";
    if (strcmp(command, "--status") == 0) return print_status();
    if (strcmp(command, "--focus") == 0) return toggle(1);
    if (strcmp(command, "--bedtime") == 0) return toggle(0);
    if (*command) {
        fputs("usage: wellbeing [--status | --focus | --bedtime]\n", stderr);
        return 2;
    }
    return platform_daemon();
}

#endif
