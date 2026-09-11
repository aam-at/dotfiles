"""Hide Kitty's bar when the focused pane runs a local multiplexer."""

import os
from os.path import basename


def foreground_executables(child_fd):
    try:
        group = os.tcgetpgrp(child_fd)
    except (OSError, TypeError):
        return
    # Kitty 0.48 misreads /proc stat names containing spaces ("tmux: client").
    # Ask the kernel for process groups instead of parsing that field ourselves.
    for entry in os.listdir("/proc"):
        if entry.isdecimal():
            try:
                if os.getpgid(int(entry)) == group:
                    yield basename(os.readlink(f"/proc/{entry}/exe"))
            except OSError:
                continue


def update_tab_bar(manager):
    window = manager.active_window
    if window is None:
        return
    # ponytail: local processes only; remote multiplexers need an explicit signal.
    hidden = any(
        executable in {"tmux", "herdr"}
        for executable in foreground_executables(window.child.child_fd)
    )
    if manager.tab_bar_hidden != hidden:
        # Kitty 0.48: per-OS-window visibility, without reloading global options.
        manager.tab_bar_hidden = hidden
        manager.mark_tab_bar_dirty()
        manager.resize()


def on_load(boss, data):
    from kitty.fast_data_types import add_timer

    def refresh(timer_id):
        for manager in boss.os_window_map.values():
            update_tab_bar(manager)

    # Check actual processes even when a shell or app doesn't emit title events.
    add_timer(refresh, 0.5, True)


if __name__ == "__main__":
    import pty
    import subprocess
    import time
    from types import SimpleNamespace

    # A real tmux client changes its process name to "tmux: client" after startup.
    name = f"kitty-tab-bar-test-{os.getpid()}"
    pid, terminal = pty.fork()
    if pid == 0:
        os.environ["TERM"] = "xterm-256color"
        os.execvp("tmux", ["tmux", "-L", name, "-f", "/dev/null", "new-session"])
    changes = []
    manager = SimpleNamespace(
        active_window=SimpleNamespace(child=SimpleNamespace(child_fd=terminal)),
        tab_bar_hidden=False,
        mark_tab_bar_dirty=lambda: changes.append("dirty"),
        resize=lambda: changes.append("resize"),
    )
    try:
        for attempt in range(50):
            with open(f"/proc/{pid}/comm") as process_name:
                if process_name.read().strip() == "tmux: client":
                    break
            time.sleep(0.1)
        else:
            raise AssertionError("tmux client did not initialize")
        update_tab_bar(manager)
        assert manager.tab_bar_hidden
        manager.name = "renamed tab"
        update_tab_bar(manager)
        assert changes == ["dirty", "resize"], changes
        manager.active_window.child.child_fd = None
        update_tab_bar(manager)
        assert not manager.tab_bar_hidden
        assert changes == ["dirty", "resize"] * 2, changes
        print("Real tmux process-name and visibility regression: PASS")
    finally:
        subprocess.run(["tmux", "-L", name, "kill-server"], check=False)
        os.close(terminal)
        os.waitpid(pid, 0)
