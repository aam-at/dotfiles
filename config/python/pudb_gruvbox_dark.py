"""
Gruvbox Dark theme for PuDB.

This file is meant to be loaded via the `custom_theme` setting by pointing
PuDB at its path. It maps the debugger UI and syntax groups onto the canonical
Gruvbox Dark palette using xterm-256 color codes.
"""

from pudb.themes.utils import add_setting, link


# Canonical Gruvbox colors mapped to their xterm-256 equivalents.
bg0 = "h235"
bg1 = "h237"
bg2 = "h239"
bg3 = "h241"
bg4 = "h243"
fg0 = "h229"
fg1 = "h223"
fg2 = "h187"
gray = "h245"

bright_red = "h203"
bright_green = "h142"
bright_yellow = "h214"
bright_blue = "h109"
bright_purple = "h175"
bright_aqua = "h108"
bright_orange = "h208"

neutral_red = "h124"
neutral_green = "h106"
neutral_yellow = "h172"
neutral_blue = "h66"
neutral_aqua = "h72"
neutral_orange = "h166"

link("current breakpoint", "current frame name")
link("focused current breakpoint", "focused current frame name")

palette.clear()
palette.update({
    # {{{ base styles
    "background": (fg1, bg0),
    "selectable": (fg0, bg1),
    "focused selectable": (fg0, bg2),
    "highlighted": (bg0, bright_aqua),
    "hotkey": (add_setting(bright_orange, "underline"), bg1),
    # }}}
    # {{{ general ui
    "input": (fg0, bg0),
    "button": (add_setting(fg0, "bold"), bg2),
    "focused button": (add_setting(bg0, "bold"), bright_aqua),
    "focused sidebar": (bg0, neutral_blue),
    "warning": (fg0, bright_red),
    "group head": (add_setting(bg0, "bold"), neutral_blue),
    "dialog title": (add_setting(fg0, "bold"), bg3),
    # }}}
    # {{{ source view
    "source": (fg0, bg0),
    "current source": (bg0, bright_yellow),
    "current focused source": (bg0, bright_orange),
    "breakpoint source": (fg0, neutral_red),
    "line number": (bg4, bg0),
    "current line marker": (add_setting(bright_yellow, "bold"), bg0),
    "breakpoint marker": (add_setting(bright_red, "bold"), bg0),
    # }}}
    # {{{ sidebar
    "sidebar one": (fg0, bg1),
    "focused sidebar one": (fg0, bg3),
    "sidebar two": (bright_blue, bg1),
    "focused sidebar two": (bright_blue, bg3),
    "sidebar three": (bright_purple, bg1),
    "focused sidebar three": (bright_purple, bg3),
    # }}}
    # {{{ variables view
    "variables": (fg0, bg1),
    "variable separator": (bg3, bg0),
    "var label": (bright_blue, bg1),
    "focused var label": (bright_blue, bg3),
    "var value": (fg0, bg0),
    "focused var value": (fg0, bg2),
    "highlighted var label": (bg0, bright_aqua),
    "highlighted var value": (bg0, bright_green),
    "focused highlighted var label": (bg0, bright_blue),
    "focused highlighted var value": (bg0, bright_green),
    "return label": (bright_green, bg0),
    "return value": (fg0, bg0),
    "focused return label": (bright_green, bg2),
    "focused return value": (fg0, bg2),
    # }}}
    # {{{ stack
    "stack": (fg0, bg1),
    "frame name": (bright_green, bg1),
    "frame class": (bright_aqua, bg1),
    "frame location": (bright_yellow, bg1),
    "focused frame name": (bright_green, bg3),
    "focused frame class": (bright_aqua, bg3),
    "focused frame location": (bright_yellow, bg3),
    "current frame name": (bright_green, bg0),
    "current frame class": (bright_aqua, bg0),
    "current frame location": (bright_yellow, bg0),
    "focused current frame name": (bright_green, bg2),
    "focused current frame class": (bright_aqua, bg2),
    "focused current frame location": (bright_yellow, bg2),
    # }}}
    # {{{ breakpoints view
    "breakpoint": (bright_orange, bg1),
    "disabled breakpoint": (gray, bg1),
    "current breakpoint": (bright_orange, bg0),
    "disabled current breakpoint": (gray, bg0),
    "focused breakpoint": (bright_orange, bg3),
    "focused current breakpoint": (bright_orange, bg2),
    "focused disabled breakpoint": (gray, bg3),
    "focused disabled current breakpoint": (gray, bg2),
    # }}}
    # {{{ shell
    "command line edit": (fg0, bg0),
    "command line prompt": (add_setting(bright_yellow, "bold"), bg0),
    "command line input": (fg0, bg0),
    "command line output": (fg2, bg0),
    "command line error": (bright_red, bg0),
    "focused command line output": (fg2, bg2),
    "focused command line error": (bright_red, bg2),
    # }}}
    # {{{ Code syntax
    "literal": (bright_purple, bg0),
    "builtin": (bright_aqua, bg0),
    "exception": (bright_orange, bg0),
    "keyword2": (bright_yellow, bg0),
    "function": (bright_blue, bg0),
    "class": (add_setting(bright_yellow, "underline"), bg0),
    "keyword": (bright_red, bg0),
    "operator": (bright_orange, bg0),
    "comment": (gray, bg0),
    "docstring": (gray, bg0),
    "argument": (bright_green, bg0),
    "pseudo": (neutral_aqua, bg0),
    "string": (bright_green, bg0),
    # }}}
})

# vim: foldmethod=marker
