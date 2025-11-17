"""
Gruvbox theme for PuDB with both Dark and Light variants.

This file is meant to be loaded via the `custom_theme` setting by pointing
PuDB at its path. Set the `PUDB_GRUVBOX_VARIANT` environment variable to
`dark` (default) or `light` before launching PuDB to pick the palette. The
colors map to the canonical Gruvbox palette using xterm-256 color codes so
the debugger matches the rest of your terminal tooling.
"""

import os

from pudb.themes.utils import add_setting, link


VARIANT_ENV = "PUDB_GRUVBOX_VARIANT"
_VARIANT_ALIASES = {
    "d": "dark",
    "dark": "dark",
    "dark-medium": "dark",
    "default": "dark",
    "l": "light",
    "light": "light",
}

_COMMON_ACCENTS = {
    "gray": "h245",
    "bright_red": "h203",
    "bright_green": "h142",
    "bright_yellow": "h214",
    "bright_blue": "h109",
    "bright_purple": "h175",
    "bright_aqua": "h108",
    "bright_orange": "h208",
    "neutral_red": "h124",
    "neutral_green": "h106",
    "neutral_yellow": "h172",
    "neutral_blue": "h66",
    "neutral_aqua": "h72",
    "neutral_orange": "h166",
}


def _merge_palette(entries):
    merged = _COMMON_ACCENTS.copy()
    merged.update(entries)
    return merged


_GRUVBOX_PALETTES = {
    "dark": _merge_palette(
        {
            "bg0": "h235",
            "bg1": "h237",
            "bg2": "h239",
            "bg3": "h241",
            "bg4": "h243",
            "fg0": "h229",
            "fg1": "h223",
            "fg2": "h187",
        }
    ),
    "light": _merge_palette(
        {
            "bg0": "h229",
            "bg1": "h223",
            "bg2": "h250",
            "bg3": "h248",
            "bg4": "h246",
            "fg0": "h237",
            "fg1": "h239",
            "fg2": "h241",
        }
    ),
}


def _resolve_variant():
    raw = os.environ.get(VARIANT_ENV, "dark")
    return _VARIANT_ALIASES.get(raw.strip().lower(), "dark")


_VARIANT = _resolve_variant()
_COLORS = _GRUVBOX_PALETTES[_VARIANT].copy()
_COLORS["accent_fg"] = (
    _COLORS["bg0"] if _VARIANT == "dark" else _COLORS["fg0"]
)

link("current breakpoint", "current frame name")
link("focused current breakpoint", "focused current frame name")

palette.clear()
palette.update({
    # {{{ base styles
    "background": (_COLORS["fg1"], _COLORS["bg0"]),
    "selectable": (_COLORS["fg0"], _COLORS["bg1"]),
    "focused selectable": (_COLORS["fg0"], _COLORS["bg2"]),
    "highlighted": (_COLORS["accent_fg"], _COLORS["bright_aqua"]),
    "hotkey": (
        add_setting(_COLORS["bright_orange"], "underline"),
        _COLORS["bg1"],
    ),
    # }}}
    # {{{ general ui
    "input": (_COLORS["fg0"], _COLORS["bg0"]),
    "button": (add_setting(_COLORS["fg0"], "bold"), _COLORS["bg2"]),
    "focused button": (
        add_setting(_COLORS["accent_fg"], "bold"),
        _COLORS["bright_aqua"],
    ),
    "focused sidebar": (_COLORS["accent_fg"], _COLORS["neutral_blue"]),
    "warning": (_COLORS["fg0"], _COLORS["bright_red"]),
    "group head": (
        add_setting(_COLORS["accent_fg"], "bold"),
        _COLORS["neutral_blue"],
    ),
    "dialog title": (
        add_setting(_COLORS["fg0"], "bold"),
        _COLORS["bg3"],
    ),
    # }}}
    # {{{ source view
    "source": (_COLORS["fg0"], _COLORS["bg0"]),
    "current source": (
        _COLORS["accent_fg"],
        _COLORS["bright_yellow"],
    ),
    "current focused source": (
        _COLORS["accent_fg"],
        _COLORS["bright_orange"],
    ),
    "breakpoint source": (_COLORS["fg0"], _COLORS["neutral_red"]),
    "line number": (_COLORS["bg4"], _COLORS["bg0"]),
    "current line marker": (
        add_setting(_COLORS["bright_yellow"], "bold"),
        _COLORS["bg0"],
    ),
    "breakpoint marker": (
        add_setting(_COLORS["bright_red"], "bold"),
        _COLORS["bg0"],
    ),
    # }}}
    # {{{ sidebar
    "sidebar one": (_COLORS["fg0"], _COLORS["bg1"]),
    "focused sidebar one": (_COLORS["fg0"], _COLORS["bg3"]),
    "sidebar two": (_COLORS["bright_blue"], _COLORS["bg1"]),
    "focused sidebar two": (_COLORS["bright_blue"], _COLORS["bg3"]),
    "sidebar three": (_COLORS["bright_purple"], _COLORS["bg1"]),
    "focused sidebar three": (_COLORS["bright_purple"], _COLORS["bg3"]),
    # }}}
    # {{{ variables view
    "variables": (_COLORS["fg0"], _COLORS["bg1"]),
    "variable separator": (_COLORS["bg3"], _COLORS["bg0"]),
    "var label": (_COLORS["bright_blue"], _COLORS["bg1"]),
    "focused var label": (_COLORS["bright_blue"], _COLORS["bg3"]),
    "var value": (_COLORS["fg0"], _COLORS["bg0"]),
    "focused var value": (_COLORS["fg0"], _COLORS["bg2"]),
    "highlighted var label": (
        _COLORS["accent_fg"],
        _COLORS["bright_aqua"],
    ),
    "highlighted var value": (
        _COLORS["accent_fg"],
        _COLORS["bright_green"],
    ),
    "focused highlighted var label": (
        _COLORS["accent_fg"],
        _COLORS["bright_blue"],
    ),
    "focused highlighted var value": (
        _COLORS["accent_fg"],
        _COLORS["bright_green"],
    ),
    "return label": (_COLORS["bright_green"], _COLORS["bg0"]),
    "return value": (_COLORS["fg0"], _COLORS["bg0"]),
    "focused return label": (_COLORS["bright_green"], _COLORS["bg2"]),
    "focused return value": (_COLORS["fg0"], _COLORS["bg2"]),
    # }}}
    # {{{ stack
    "stack": (_COLORS["fg0"], _COLORS["bg1"]),
    "frame name": (_COLORS["bright_green"], _COLORS["bg1"]),
    "frame class": (_COLORS["bright_aqua"], _COLORS["bg1"]),
    "frame location": (_COLORS["bright_yellow"], _COLORS["bg1"]),
    "focused frame name": (_COLORS["bright_green"], _COLORS["bg3"]),
    "focused frame class": (_COLORS["bright_aqua"], _COLORS["bg3"]),
    "focused frame location": (_COLORS["bright_yellow"], _COLORS["bg3"]),
    "current frame name": (_COLORS["bright_green"], _COLORS["bg0"]),
    "current frame class": (_COLORS["bright_aqua"], _COLORS["bg0"]),
    "current frame location": (_COLORS["bright_yellow"], _COLORS["bg0"]),
    "focused current frame name": (_COLORS["bright_green"], _COLORS["bg2"]),
    "focused current frame class": (_COLORS["bright_aqua"], _COLORS["bg2"]),
    "focused current frame location": (
        _COLORS["bright_yellow"],
        _COLORS["bg2"],
    ),
    # }}}
    # {{{ breakpoints view
    "breakpoint": (_COLORS["bright_orange"], _COLORS["bg1"]),
    "disabled breakpoint": (_COLORS["gray"], _COLORS["bg1"]),
    "current breakpoint": (_COLORS["bright_orange"], _COLORS["bg0"]),
    "disabled current breakpoint": (_COLORS["gray"], _COLORS["bg0"]),
    "focused breakpoint": (_COLORS["bright_orange"], _COLORS["bg3"]),
    "focused current breakpoint": (_COLORS["bright_orange"], _COLORS["bg2"]),
    "focused disabled breakpoint": (_COLORS["gray"], _COLORS["bg3"]),
    "focused disabled current breakpoint": (
        _COLORS["gray"],
        _COLORS["bg2"],
    ),
    # }}}
    # {{{ shell
    "command line edit": (_COLORS["fg0"], _COLORS["bg0"]),
    "command line prompt": (
        add_setting(_COLORS["bright_yellow"], "bold"),
        _COLORS["bg0"],
    ),
    "command line input": (_COLORS["fg0"], _COLORS["bg0"]),
    "command line output": (_COLORS["fg2"], _COLORS["bg0"]),
    "command line error": (_COLORS["bright_red"], _COLORS["bg0"]),
    "focused command line output": (_COLORS["fg2"], _COLORS["bg2"]),
    "focused command line error": (_COLORS["bright_red"], _COLORS["bg2"]),
    # }}}
    # {{{ Code syntax
    "literal": (_COLORS["bright_purple"], _COLORS["bg0"]),
    "builtin": (_COLORS["bright_aqua"], _COLORS["bg0"]),
    "exception": (_COLORS["bright_orange"], _COLORS["bg0"]),
    "keyword2": (_COLORS["bright_yellow"], _COLORS["bg0"]),
    "function": (_COLORS["bright_blue"], _COLORS["bg0"]),
    "class": (
        add_setting(_COLORS["bright_yellow"], "underline"),
        _COLORS["bg0"],
    ),
    "keyword": (_COLORS["bright_red"], _COLORS["bg0"]),
    "operator": (_COLORS["bright_orange"], _COLORS["bg0"]),
    "comment": (_COLORS["gray"], _COLORS["bg0"]),
    "docstring": (_COLORS["gray"], _COLORS["bg0"]),
    "argument": (_COLORS["bright_green"], _COLORS["bg0"]),
    "pseudo": (_COLORS["neutral_aqua"], _COLORS["bg0"]),
    "string": (_COLORS["bright_green"], _COLORS["bg0"]),
    # }}}
})

# vim: foldmethod=marker
