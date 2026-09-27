# wellbeing on Linux: make, make test, make install (dotbot runs install via
# install.conf.yaml). On Windows, windots' setup builds wellbeing.exe.

CC ?= cc
# Display strings are truncated on purpose when they don't fit.
CFLAGS ?= -O2 -Wall -Wextra -Wno-format-truncation
LIBDIR ?= $(HOME)/.local/lib/wellbeing
BINDIR ?= $(HOME)/.local/bin
AW_SERVER_CONFIG ?= $(HOME)/.config/activitywatch/aw-server/aw-server.toml
SOURCES = wellbeing.c platform-linux.h ../lib/http.h ../lib/json.h ../lib/jsmn.h

all: wellbeing

wellbeing: $(SOURCES)
	$(CC) $(CFLAGS) -s -o $@ wellbeing.c -lm

test: wellbeing.test
	./wellbeing.test

wellbeing.test: wellbeing.test.c $(SOURCES)
	$(CC) $(CFLAGS) -o $@ wellbeing.test.c -lm

# The binary and the Hyprland shader beside it (it looks there), a command
# on PATH for keybinds (wellbeing --focus), and the dashboard served by
# aw-server at http://127.0.0.1:5600/pages/wellbeing/.
install: wellbeing
	install -Dm755 wellbeing $(LIBDIR)/wellbeing
	install -Dm644 grayscale.frag $(LIBDIR)/grayscale.frag
	mkdir -p $(BINDIR) && ln -sf $(LIBDIR)/wellbeing $(BINDIR)/wellbeing
	./serve-dashboard.sh "$(AW_SERVER_CONFIG)" "$(CURDIR)/dashboard"

clean:
	rm -f wellbeing wellbeing.test

.PHONY: all test install clean
