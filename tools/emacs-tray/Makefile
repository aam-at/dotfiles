# emacs-tray on Linux: make, make test, make install (dotbot runs install via
# install.conf.yaml). Windows has its own in windots' yasb/emacs.

CC ?= cc
CFLAGS ?= -O2 -Wall -Wextra
LIBDIR ?= $(HOME)/.local/lib/emacs-tray
SYSTEMD_LIBS ?= $(shell pkg-config --libs libsystemd 2>/dev/null || echo -lsystemd)

all: emacs-tray

emacs-tray: emacs-tray.c
	$(CC) $(CFLAGS) -s -o $@ emacs-tray.c $(SYSTEMD_LIBS)

test: emacs-tray.test
	./emacs-tray.test

emacs-tray.test: emacs-tray.test.c emacs-tray.c
	$(CC) $(CFLAGS) -o $@ emacs-tray.test.c

# The binary and the icons beside it (it looks there).
install: emacs-tray
	install -Dm755 emacs-tray $(LIBDIR)/emacs-tray
	install -d $(LIBDIR)/icons
	install -m644 icons/*.png $(LIBDIR)/icons/

clean:
	rm -f emacs-tray emacs-tray.test

.PHONY: all test install clean
