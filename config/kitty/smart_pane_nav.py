# vim-aware pane navigation.
#
# ctrl+h/j/k/l normally focus the neighboring kitty window (tmux-style).
# But LazyVim's vim-tmux-navigator plugin (config/lazyvim/lua/plugins/extras.lua)
# maps the same keys *inside* vim to move between vim's own splits, falling
# back to `kitty @ kitten neighboring_window.py <dir>` when vim has no more
# splits in that direction. For that to work, kitty must NOT swallow the key
# when the foreground process is vim/nvim -- it has to forward the raw
# keypress through so vim sees it first.
#
# So: if the focused pane's foreground process is vim/nvim, forward the key
# unchanged (same re-encode trick as smart_scroll.py). Otherwise, do kitty's
# own neighboring_window directly.

import re

import kitty.key_encoding as ke

VIM_RE = re.compile(r"(?:^|/)n?vim(?:\.[^/]*)?$")


def main(args):
    pass


def is_vim(w):
    if w is None:
        return False
    cmdline = w.child.foreground_cmdline
    return bool(cmdline) and bool(VIM_RE.search(cmdline[0]))


def handle_result(args, result, target_window_id, boss):
    w = boss.window_id_map.get(target_window_id)
    if w is None:
        return

    direction, shortcut = args[1], args[2]

    if not is_vim(w):
        boss.active_tab.neighboring_window(direction)
        return

    mods, key = ke.parse_shortcut(shortcut)
    shift, alt, ctrl, super, hyper, meta, caps_lock, num_lock = (
        bool(mods & bit) for bit in (
            ke.SHIFT, ke.ALT, ke.CTRL, ke.SUPER,
            ke.HYPER, ke.META, ke.CAPS_LOCK, ke.NUM_LOCK))
    for action in (ke.EventType.PRESS, ke.EventType.RELEASE):
        key_event = ke.KeyEvent(
            type=action, mods=mods, key=key,
            shift=shift, alt=alt, ctrl=ctrl, super=super,
            hyper=hyper, meta=meta, caps_lock=caps_lock, num_lock=num_lock)
        window_system_event = key_event.as_window_system_event()
        sequence = w.encoded_key(window_system_event)
        w.write_to_child(sequence)


handle_result.no_ui = True
