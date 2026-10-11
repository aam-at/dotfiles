/*
 * Tests for emacs-tray.c's daemon state, icon choice and menu. Run by
 * tools/test.sh, or by hand: make test
 */
#ifdef __linux__
#define EMACS_TRAY_TEST
#include "emacs-tray.c"

static int failures;

static void check(int line, int ok) {
    if (ok) return;
    printf("emacs-tray.test.c:%d: failed\n", line);
    failures++;
}

int main(void) {
    /* Up once the forking unit is active; loading counts as starting. */
    check(__LINE__, state_from_active("active") == RUNNING);
    check(__LINE__, state_from_active("reloading") == RUNNING);
    check(__LINE__, state_from_active("activating") == STARTING);
    check(__LINE__, state_from_active("inactive") == STOPPED);
    check(__LINE__, state_from_active("deactivating") == STOPPED);
    check(__LINE__, state_from_active("failed") == STOPPED);

    /* A spin frame every FRAME_MS, around again after FRAMES of them. */
    check(__LINE__, icon_index(RUNNING, 12345) == ICON_COLOR);
    check(__LINE__, icon_index(STOPPED, 12345) == ICON_OFF);
    check(__LINE__, icon_index(STARTING, 0) == ICON_SPIN);
    check(__LINE__, icon_index(STARTING, FRAME_MS * 3 + 1) == ICON_SPIN + 3);
    check(__LINE__, icon_index(STARTING, FRAME_MS * FRAMES) == ICON_SPIN);

    char name[64];
    icon_name("doom", ICON_COLOR, name, sizeof name);
    check(__LINE__, !strcmp(name, "emacs-tray-doom"));
    icon_name("doom", ICON_OFF, name, sizeof name);
    check(__LINE__, !strcmp(name, "emacs-tray-doom-off"));
    icon_name("spacemacs", ICON_SPIN + 11, name, sizeof name);
    check(__LINE__, !strcmp(name, "emacs-tray-spacemacs-spin11"));

    /* The menu: Start only when stopped, Stop only when up, nothing but those
       two answer while starting. */
    check(__LINE__, item_enabled(ITEM_OPEN, STOPPED) && item_enabled(ITEM_OPEN, RUNNING));
    check(__LINE__, !item_enabled(ITEM_OPEN, STARTING));
    check(__LINE__, item_enabled(ITEM_START, STOPPED));
    check(__LINE__,
          !item_enabled(ITEM_START, STARTING) && !item_enabled(ITEM_START, RUNNING));
    check(__LINE__, item_enabled(ITEM_STOP, RUNNING));
    check(__LINE__,
          !item_enabled(ITEM_STOP, STARTING) && !item_enabled(ITEM_STOP, STOPPED));
    check(__LINE__,
          item_enabled(ITEM_RESTART, STOPPED) && item_enabled(ITEM_RESTART, RUNNING));
    check(__LINE__, !item_enabled(ITEM_RESTART, STARTING));
    return failures != 0;
}
#else
int main(void) { return 0; }
#endif
