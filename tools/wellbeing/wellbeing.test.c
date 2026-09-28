/*
 * Tests for wellbeing.c's logic, on Windows and Linux alike:
 *   make test                                   (Linux)
 *   gcc -Wall -o t.exe wellbeing.test.c -lws2_32 && t.exe   (Windows)
 *   ... --live [port]   also today's usage and the settings from a running
 *                       aw-server (read-only; default port 5600)
 */
#define WELLBEING_TEST
#include "wellbeing.c"

static int failures;

#define CHECK(condition) do { if (!(condition)) { printf("wellbeing.test.c:%d: failed: %s\n", __LINE__, #condition); failures++; } } while (0)

static void check_str(int line, const char *actual, const char *expected) {
    if (strcmp(actual, expected) == 0) return;
    printf("wellbeing.test.c:%d: got  \"%s\"\n                    want \"%s\"\n", line, actual, expected);
    failures++;
}
#define CHECK_STR(actual, expected) check_str(__LINE__, actual, expected)

static void test_text(void) {
    char out[32];
    format_duration(0, out, sizeof out), CHECK_STR(out, "0m");
    format_duration(59 * 60 + 59, out, sizeof out), CHECK_STR(out, "59m");
    format_duration(3600, out, sizeof out), CHECK_STR(out, "1h 0m");
    format_duration(2 * 3600 + 5 * 60, out, sizeof out), CHECK_STR(out, "2h 5m");
    CHECK(parse_time("23:30") == 1410 && parse_time("07:00") == 420 && parse_time("0:05") == 5);
    CHECK(parse_time("24:00") == -1 && parse_time("soon") == -1);
    CHECK(ci_equal("MSEdge.EXE", "msedge.exe") && !ci_equal("msedge", "msedge.exe") && !ci_equal("a@", "a`"));
    display_name(KIND_APP, "Discord.exe", out, sizeof out), CHECK_STR(out, "Discord");
    display_name(KIND_APP, "firefox", out, sizeof out), CHECK_STR(out, "firefox");
    display_name(KIND_SITE, "youtube.com", out, sizeof out), CHECK_STR(out, "youtube.com");
}

static const char *settings =
    "{\"other\": {\"x\": [1, {\"y\": 2}]},"
    " \"wellbeing\": {"
    "   \"limits\": [{\"kind\": \"site\", \"name\": \"youtube.com\", \"minutes\": 30},"
    "                {\"kind\": \"app\", \"name\": \"Discord.exe\", \"minutes\": 90},"
    "                {\"kind\": \"app\", \"name\": \"\", \"minutes\": 5},"
    "                {\"kind\": \"site\", \"name\": \"x.com\", \"minutes\": 0}],"
    "   \"focus\": [{\"kind\": \"site\", \"name\": \"youtube.com\"}, {\"kind\": \"app\", \"name\": \"kitty\"}],"
    "   \"bedtime\": {\"enabled\": true, \"start\": \"23:30\", \"end\": \"07:00\"},"
    "   \"commands\": {\"dnd_on\": \"makoctl mode -a \\\"dnd\\\"\", \"grayscale_on\": \"\"}},"
    " \"wellbeing.state\": {\"focus_mode\": true, \"bedtime_paused\": \"2026-09-27\"}}";

static void test_settings(void) {
    Config c;
    State s;
    CHECK(parse_settings(settings, &c, NULL, &s));
    CHECK(c.limit_count == 2); /* empty names and 0 minutes are skipped */
    CHECK(c.limits[0].kind == KIND_SITE && strcmp(c.limits[0].name, "youtube.com") == 0 && c.limits[0].minutes == 30 && c.limits[0].next_s < 0);
    CHECK(c.limits[1].kind == KIND_APP && strcmp(c.limits[1].name, "Discord.exe") == 0 && c.limits[1].minutes == 90);
    CHECK(c.focus_count == 2 && c.focus[1].kind == KIND_APP && strcmp(c.focus[1].name, "kitty") == 0);
    CHECK(c.bedtime_start == 1410 && c.bedtime_end == 420);
    CHECK_STR(c.dnd_on, "makoctl mode -a \"dnd\"");
    CHECK_STR(c.grayscale_on, "");
    CHECK(s.focus_mode == 1);
    CHECK_STR(s.bedtime_paused, "2026-09-27");

    /* A reload keeps nudge progress for unchanged limits only. */
    c.limits[0].next_s = 1800;
    c.limits[1].next_s = 5100;
    c.focus[0].last_nudge = 42;
    Config reloaded;
    parse_config("{\"limits\": [{\"kind\": \"site\", \"name\": \"YouTube.com\", \"minutes\": 30}, {\"kind\": \"app\", \"name\": \"Discord.exe\", \"minutes\": 60}],"
                 " \"focus\": [{\"kind\": \"site\", \"name\": \"youtube.com\"}]}", &reloaded, &c);
    CHECK(reloaded.limits[0].next_s == 1800 && reloaded.limits[1].next_s < 0);
    CHECK(reloaded.focus[0].last_nudge == 42);

    /* Nothing set yet, or bedtime disabled or half set: all off. */
    CHECK(parse_settings("{}", &c, NULL, &s));
    CHECK(c.limit_count == 0 && c.focus_count == 0 && c.bedtime_start == -1 && s.focus_mode == 0 && !*s.bedtime_paused);
    parse_config("{\"bedtime\": {\"enabled\": false, \"start\": \"23:30\", \"end\": \"07:00\"}}", &c, NULL);
    CHECK(c.bedtime_start == -1);
    parse_config("{\"bedtime\": {\"enabled\": true, \"start\": \"23:30\"}}", &c, NULL);
    CHECK(c.bedtime_start == -1);
    parse_config(NULL, &c, NULL);
    CHECK(c.limit_count == 0);
    CHECK(!parse_settings("not json", &c, NULL, &s) && !parse_settings("[1]", &c, NULL, &s));

    char json[128];
    State state = {1, "2026-09-28", 1790560800};
    state_json(&state, json, sizeof json);
    CHECK_STR(json, "{\"focus_mode\": true, \"bedtime_paused\": \"2026-09-28\", \"bedtime_until\": 1790560800}");
    parse_state(json, &s);
    CHECK(s.focus_mode == 1 && strcmp(s.bedtime_paused, "2026-09-28") == 0 && s.bedtime_until == 1790560800);
}

static void test_matching(void) {
    CHECK(domain_matches("youtube.com", "youtube.com") && domain_matches("m.youtube.com", "youtube.com"));
    CHECK(domain_matches("calendar.google.com", "google.com") && domain_matches("YouTube.com", "youtube.com"));
    CHECK(!domain_matches("notyoutube.com", "youtube.com") && !domain_matches("youtube.com.evil.net", "youtube.com"));
    CHECK(!domain_matches("youtube.com", ""));
    CHECK(rule_matches(KIND_APP, "kitty", "kitty", "") && !rule_matches(KIND_APP, "kitty", "kitty-dev", ""));
    CHECK(rule_matches(KIND_SITE, "youtube.com", "firefox", "m.youtube.com") && !rule_matches(KIND_SITE, "youtube.com", "firefox", ""));
    CHECK(is_browser("msedge.exe") && is_browser("Chrome.exe") && is_browser("firefox") && is_browser("google-chrome") && !is_browser("kitty"));

    char domain[64];
    domain_of("https://www.youtube.com/watch?v=1", domain, sizeof domain), CHECK_STR(domain, "youtube.com");
    domain_of("http://127.0.0.1:5600/pages/wellbeing/", domain, sizeof domain), CHECK_STR(domain, "127.0.0.1");
    domain_of("edge://newtab/", domain, sizeof domain), CHECK_STR(domain, "newtab");
    domain_of("https://m.youtube.com#x", domain, sizeof domain), CHECK_STR(domain, "m.youtube.com");
}

/* As aw-server returns the usage query: keys sorted, "data" first. */
static const char *response =
    "[{\"apps\": ["
    "{\"data\": {\"app\": \"WindowsTerminal.exe\"}, \"duration\": 3000.25, \"id\": null, \"timestamp\": \"2026-09-28T01:00:00+00:00\"}, "
    "{\"data\": {\"app\": \"msedge.exe\"}, \"duration\": 2400.0, \"id\": null, \"timestamp\": \"2026-09-28T02:00:00+00:00\"}], "
    "\"sites\": ["
    "{\"data\": {\"$domain\": \"youtube.com\"}, \"duration\": 1200, \"id\": null, \"timestamp\": \"2026-09-28T02:00:00+00:00\"}, "
    "{\"data\": {\"$domain\": \"m.youtube.com\"}, \"duration\": 300, \"id\": null, \"timestamp\": \"2026-09-28T02:30:00+00:00\"}, "
    "{\"data\": {\"$domain\": \"calendar.google.com\"}, \"duration\": 600, \"id\": null, \"timestamp\": \"2026-09-28T02:40:00+00:00\"}], "
    "\"total\": 5400.5}]";

static void test_usage(void) {
    Usage u;
    CHECK(parse_usage(response, &u));
    CHECK(u.total == 5400.5 && u.app_count == 2 && u.site_count == 3);
    CHECK_STR(u.apps[0].name, "WindowsTerminal.exe");
    CHECK(u.apps[0].seconds == 3000.25 && u.apps[1].seconds == 2400);
    CHECK(rule_usage(&u, KIND_SITE, "youtube.com") == 1500);
    CHECK(rule_usage(&u, KIND_APP, "MSEDGE.exe") == 2400);
    CHECK(rule_usage(&u, KIND_SITE, "google.com") == 600 && rule_usage(&u, KIND_SITE, "reddit.com") == 0);

    /* Insertion order, "duration" first: the same. */
    Usage unsorted;
    CHECK(parse_usage("[{\"total\": 60, \"apps\": [{\"id\": 1, \"duration\": 60, \"data\": {\"app\": \"kitty\"}}]}]", &unsorted));
    CHECK(unsorted.app_count == 1 && unsorted.apps[0].seconds == 60 && strcmp(unsorted.apps[0].name, "kitty") == 0 && unsorted.site_count == 0);
    CHECK(!parse_usage("{\"message\": \"error\"}", &u) && !parse_usage("", &u));

    char json[256];
    parse_usage(response, &u);
    status_json(&u, 0, "T", "no activity yet", json, sizeof json);
    CHECK_STR(json, "{\"icon\": \"T\", \"total\": \"1h 30m\", \"top\": \"WindowsTerminal 50m\", \"focus\": \"off\"}");
    Usage none = {0};
    status_json(&none, 1, "F", "wellbeing is not running", json, sizeof json);
    CHECK_STR(json, "{\"icon\": \"F\", \"total\": \"--\", \"top\": \"wellbeing is not running\", \"focus\": \"on\"}");

    /* The query is JSON aw-server can take. */
    char body[4096], period[64];
    today_period(time(NULL), period, sizeof period);
    CHECK(strlen(period) == 41 && period[20] == '/');
    usage_query(period, 1, body, sizeof body);
    Json json_body;
    CHECK(json_parse(body, &json_body) && json_size(&json_body, json_get(&json_body, 0, "query")) == 6);
    json_free(&json_body);
    usage_query(period, 0, body, sizeof body);
    CHECK(json_parse(body, &json_body) && json_size(&json_body, json_get(&json_body, 0, "query")) == 4);
    json_free(&json_body);
}

static void test_limit_nudges(void) {
    Limit l = {KIND_SITE, "youtube.com", 30, -1};
    /* The first reading only sets the next point: no nudge on start. */
    CHECK(check_limit(&l, 0) == NUDGE_NONE && l.next_s == 1500);
    CHECK(check_limit(&l, 1499) == NUDGE_NONE);
    CHECK(check_limit(&l, 1500) == NUDGE_WARNING && l.next_s == 1800);
    CHECK(check_limit(&l, 1800) == NUDGE_REACHED && l.next_s == 3600);
    CHECK(check_limit(&l, 3600) == NUDGE_OVER && l.next_s == 5400);
    CHECK(check_limit(&l, 5500) == NUDGE_OVER && l.next_s == 7200);
    /* Restarted over the limit: silent until the next 30-minute mark. */
    Limit restarted = {KIND_SITE, "youtube.com", 30, -1};
    CHECK(check_limit(&restarted, 2000) == NUDGE_NONE && restarted.next_s == 3600);
    /* Several points passed in one reading: one nudge. */
    Limit jumped = {KIND_SITE, "youtube.com", 30, -1};
    check_limit(&jumped, 0);
    CHECK(check_limit(&jumped, 4000) == NUDGE_OVER && jumped.next_s == 5400);
    /* Short limits get no 5-minute warning. */
    Limit short_limit = {KIND_SITE, "x.com", 5, -1};
    CHECK(check_limit(&short_limit, 0) == NUDGE_NONE && short_limit.next_s == 300);
    CHECK(check_limit(&short_limit, 300) == NUDGE_REACHED);

    char title[160], body[256];
    nudge_text(&(Limit){KIND_SITE, "youtube.com", 30, -1}, NUDGE_WARNING, 1500, title, sizeof title, body, sizeof body);
    CHECK_STR(title, "youtube.com: 5 minutes left");
    CHECK_STR(body, "25m of your 30m daily limit used.");
    nudge_text(&(Limit){KIND_APP, "Discord.exe", 90, -1}, NUDGE_REACHED, 5400, title, sizeof title, body, sizeof body);
    CHECK_STR(title, "Discord: daily limit reached");
    nudge_text(&(Limit){KIND_APP, "discord", 90, -1}, NUDGE_OVER, 7300, title, sizeof title, body, sizeof body);
    CHECK_STR(title, "discord: 31m over your limit");
    CHECK_STR(body, "2h 1m today, of 1h 30m.");
}

static void test_bedtime(void) {
    int start = 1410, end = 420; /* 23:30 to 07:00 */
    CHECK(!in_bedtime(1409, start, end) && in_bedtime(1410, start, end) && in_bedtime(180, start, end));
    CHECK(in_bedtime(419, start, end) && !in_bedtime(420, start, end) && !in_bedtime(720, start, end));
    CHECK(in_bedtime(13 * 60, 13 * 60, 14 * 60) && !in_bedtime(14 * 60, 13 * 60, 14 * 60));
    CHECK(!in_bedtime(0, -1, -1));

    /* After midnight, a bedtime belongs to the day it started. */
    struct tm local = {0};
    local.tm_year = 2026 - 1900, local.tm_mon = 8, local.tm_mday = 28, local.tm_hour = 2, local.tm_isdst = -1;
    char date[16];
    bedtime_date(mktime(&local), 1410, date, sizeof date), CHECK_STR(date, "2026-09-27");
    local.tm_hour = 23, local.tm_min = 45;
    bedtime_date(mktime(&local), 1410, date, sizeof date), CHECK_STR(date, "2026-09-28");
}

/* A local time on 28 September 2026 (hour, minute). */
static time_t at(int day, int hour, int minute) {
    struct tm local = {0};
    local.tm_year = 2026 - 1900, local.tm_mon = 8, local.tm_mday = day, local.tm_hour = hour, local.tm_min = minute, local.tm_isdst = -1;
    return mktime(&local);
}

/* The bedtime key: on now or off, with and without a schedule. */
static void test_bedtime_toggle(void) {
    Config none;
    parse_config("{}", &none, NULL);
    State s = {0};
    time_t evening = at(28, 20, 0);
    CHECK(!bedtime_active(&s, &none, evening));
    /* No schedule: on until 07:00 tomorrow, then off again. */
    CHECK(toggle_bedtime(&s, &none, evening) == BEDTIME_STARTED);
    CHECK(s.bedtime_until == at(29, 7, 0) && bedtime_active(&s, &none, evening));
    CHECK(bedtime_end_minute(&s, &none, evening) == 7 * 60);
    CHECK(bedtime_active(&s, &none, at(29, 6, 59)) && !bedtime_active(&s, &none, at(29, 7, 0)));
    CHECK(toggle_bedtime(&s, &none, evening) == BEDTIME_ENDED);
    CHECK(s.bedtime_until == 0 && !bedtime_active(&s, &none, evening));
    /* After midnight, on until 07:00 the same morning. */
    toggle_bedtime(&s, &none, at(28, 1, 30));
    CHECK(s.bedtime_until == at(28, 7, 0));

    Config scheduled;
    parse_config("{\"bedtime\": {\"enabled\": true, \"start\": \"23:30\", \"end\": \"06:30\"}}", &scheduled, NULL);
    State t = {0};
    /* Scheduled and on: the key pauses it until the next night... */
    time_t night = at(29, 2, 0);
    CHECK(bedtime_active(&t, &scheduled, night));
    CHECK(toggle_bedtime(&t, &scheduled, night) == BEDTIME_PAUSED);
    CHECK(strcmp(t.bedtime_paused, "2026-09-28") == 0 && !bedtime_active(&t, &scheduled, night));
    CHECK(bedtime_active(&t, &scheduled, at(29, 23, 45)));
    /* ...and again resumes it. */
    CHECK(toggle_bedtime(&t, &scheduled, night) == BEDTIME_RESUMED);
    CHECK(bedtime_active(&t, &scheduled, night) && !*t.bedtime_paused);
    /* Outside the schedule: on now, until the schedule's end time. */
    State u = {0};
    CHECK(toggle_bedtime(&u, &scheduled, evening) == BEDTIME_STARTED);
    CHECK(u.bedtime_until == at(29, 6, 30) && bedtime_end_minute(&u, &scheduled, evening) == 6 * 60 + 30);
    /* Turned on by hand, then the schedule starts: the key turns it all off. */
    CHECK(toggle_bedtime(&u, &scheduled, at(28, 23, 40)) == BEDTIME_PAUSED);
    CHECK(u.bedtime_until == 0 && !bedtime_active(&u, &scheduled, at(28, 23, 40)));
}

static void test_json(void) {
    Json json;
    CHECK(json_parse("{\"a\": \"caf\\u00e9 \\\"x\\\" \\ud83c\\udfb5\", \"b\": [true, false, null, -2.5]}", &json));
    char text[64];
    json_string(&json, json_get(&json, 0, "a"), text, sizeof text);
    CHECK_STR(text, "caf\xC3\xA9 \"x\" \xF0\x9F\x8E\xB5");
    int b = json_get(&json, 0, "b");
    CHECK(json_size(&json, b) == 4 && json_bool(&json, json_at(&json, b, 0)) && !json_bool(&json, json_at(&json, b, 1)));
    CHECK(json_number(&json, json_at(&json, b, 3), 0) == -2.5 && json_number(&json, json_at(&json, b, 2), 7) == 7);
    CHECK(json_get(&json, 0, "missing") == -1 && json_at(&json, b, 9) == -1);
    json_free(&json);
    json_escape("say \"hi\"\n", text, sizeof text);
    CHECK_STR(text, "say \\\"hi\\\"\\u000a");
}

int main(int argc, char **argv) {
    test_text();
    test_settings();
    test_matching();
    test_usage();
    test_limit_nudges();
    test_bedtime();
    test_bedtime_toggle();
    test_json();
    if (argc > 1 && strcmp(argv[1], "--live") == 0) {
        if (argc > 2) http_port = atoi(argv[2]);
        static char body[4096], reply[1 << 17];
        char period[64];
        today_period(time(NULL), period, sizeof period);
        Usage u = {0};
        for (int with_web = 1; with_web >= 0 && !u.valid; with_web--) {
            usage_query(period, with_web, body, sizeof body);
            if (http_request("POST", "/api/0/query/", body, reply, sizeof reply) == 200) parse_usage(reply, &u);
        }
        Config c;
        State s;
        int settings_ok = http_request("GET", "/api/0/settings", NULL, reply, sizeof reply) == 200 && parse_settings(reply, &c, NULL, &s);
        if (!u.valid || !settings_ok) {
            printf("live: aw-server on port %d: usage %s, settings %s\n", http_port, u.valid ? "ok" : "FAILED", settings_ok ? "ok" : "FAILED");
            failures++;
        }
        else {
            char json[256];
            status_json(&u, s.focus_mode, "*", "no activity yet", json, sizeof json);
            printf("live: %s\n  %d apps, %d sites; %d limits, %d focus rules, bedtime %s\n", json, u.app_count, u.site_count, c.limit_count, c.focus_count,
                c.bedtime_start >= 0 ? "on" : "off");
        }
    }
    if (failures) printf("%d wellbeing test(s) failed\n", failures);
    else printf("wellbeing tests passed\n");
    return failures != 0;
}
