#!/usr/bin/env python3
"""COSMIC-styled session menu for the shared Super+X binding."""

from __future__ import annotations

import getpass
import re
import socket
import subprocess
from pathlib import Path

import gi

gi.require_version("Gtk", "4.0")
gi.require_version("Gdk", "4.0")
from gi.repository import Gdk, Gio, Gtk  # noqa: E402


APP_ID = "dev.dotfiles.CosmicSessionMenu"


def portal_prefers_dark() -> bool:
    """Read the standard desktop color-scheme portal; default to COSMIC dark."""
    try:
        result = subprocess.run(
            [
                "gdbus",
                "call",
                "--session",
                "--dest",
                "org.freedesktop.portal.Desktop",
                "--object-path",
                "/org/freedesktop/portal/desktop",
                "--method",
                "org.freedesktop.portal.Settings.Read",
                "org.freedesktop.appearance",
                "color-scheme",
            ],
            capture_output=True,
            check=False,
            text=True,
            timeout=1,
        )
        # xdg-desktop-portal: 1 = prefer dark, 2 = prefer light, 0 = no preference.
        match = re.search(r"uint32\s+(\d+)", result.stdout)
        if match:
            return match.group(1) != "2"
    except (OSError, subprocess.SubprocessError):
        pass
    return True


def ron_color(path: Path, key: str, fallback: str) -> str:
    """Read a #RRGGBBAA color from one of COSMIC's tracked RON theme files."""
    try:
        match = re.search(
            rf"^\s*{re.escape(key)}:\s*\"(#[0-9A-Fa-f]{{8}})\"",
            path.read_text(),
            re.MULTILINE,
        )
    except OSError:
        return fallback
    if not match:
        return fallback

    rgba = match.group(1)
    # GTK CSS accepts #RRGGBBAA, but stripping a fully opaque alpha makes the
    # generated stylesheet easier to inspect and works on older GTK 4 builds.
    return rgba[:7] if rgba.endswith("FF") else rgba


def ron_component_color(path: Path, key: str, fallback: str) -> str:
    """Read a color from a theme's nested component palette."""
    try:
        text = path.read_text()
        component = re.search(r"component:\s*\((.*?)\n\s*\),", text, re.DOTALL)
        if component is None:
            return fallback
        match = re.search(
            rf"^\s*{re.escape(key)}:\s*\"(#[0-9A-Fa-f]{{8}})\"",
            component.group(1),
            re.MULTILINE,
        )
    except OSError:
        return fallback
    if not match:
        return fallback
    rgba = match.group(1)
    return rgba[:7] if rgba.endswith("FF") else rgba


def theme_colors() -> dict[str, str]:
    mode = "Dark" if portal_prefers_dark() else "Light"
    root = Path.home() / ".config" / "cosmic" / f"com.system76.CosmicTheme.{mode}" / "v2"

    dark = mode == "Dark"
    return {
        "card": ron_color(root / "primary", "base", "#272727" if dark else "#EBEBEB"),
        "button": ron_component_color(
            root / "primary", "base", "#363636" if dark else "#DADADA"
        ),
        "button_hover": ron_component_color(
            root / "primary", "hover", "#4A4A4A" if dark else "#DEDEDE"
        ),
        "text": ron_color(root / "primary", "on", "#FFFFFF" if dark else "#000000"),
        "muted": "#B8B8B8" if dark else "#565656",
        "accent": ron_color(root / "accent", "base", "#63D0DF" if dark else "#00525A"),
        "destructive": ron_color(
            root / "destructive", "base", "#FFA09A" if dark else "#890418"
        ),
        "destructive_on": ron_color(
            root / "destructive", "on", "#000000" if dark else "#FFFFFF"
        ),
        "overlay": "rgba(0, 0, 0, 0.52)" if dark else "rgba(20, 20, 20, 0.30)",
    }


class SessionMenu(Gtk.Application):
    def __init__(self) -> None:
        super().__init__(application_id=APP_ID, flags=Gio.ApplicationFlags.DEFAULT_FLAGS)
        self.window: Gtk.ApplicationWindow | None = None

    def do_activate(self) -> None:
        # Gtk.Application is single-instance. Pressing Super+X again sends
        # another activation to the existing process, which makes this a toggle.
        if self.window is not None and self.window.get_visible():
            self.window.close()
            self.quit()
            return

        colors = theme_colors()
        self.install_css(colors)

        window = Gtk.ApplicationWindow(application=self)
        self.window = window
        window.set_title("Session")
        window.set_decorated(False)
        window.fullscreen()
        window.add_css_class("session-overlay")
        window.connect("close-request", self.on_close_request)

        key_controller = Gtk.EventControllerKey()
        key_controller.connect("key-pressed", self.on_key_pressed)
        window.add_controller(key_controller)

        outer = Gtk.Box(orientation=Gtk.Orientation.VERTICAL)
        outer.set_halign(Gtk.Align.CENTER)
        outer.set_valign(Gtk.Align.CENTER)

        card = Gtk.Box(orientation=Gtk.Orientation.VERTICAL, spacing=22)
        card.add_css_class("session-card")
        outer.append(card)

        heading = Gtk.Label(label="Session")
        heading.set_halign(Gtk.Align.START)
        heading.add_css_class("session-title")
        card.append(heading)

        identity = f"{getpass.getuser()}  ·  {socket.gethostname()}"
        subtitle = Gtk.Label(label=identity)
        subtitle.set_halign(Gtk.Align.START)
        subtitle.add_css_class("session-subtitle")
        card.append(subtitle)

        actions = Gtk.Box(orientation=Gtk.Orientation.HORIZONTAL, spacing=12)
        actions.set_homogeneous(True)
        card.append(actions)

        buttons = [
            self.action_button("Lock", "Alt+L", "system-lock-screen-symbolic", ["loginctl", "lock-session"]),
            self.action_button("Suspend", "Alt+S", "system-suspend-symbolic", ["systemctl", "suspend"]),
            self.action_button("Log out", "Alt+O", "system-log-out-symbolic", ["cosmic-osd", "log-out"]),
            self.action_button("Restart", "Alt+R", "system-reboot-symbolic", ["cosmic-osd", "restart"]),
            self.action_button(
                "Shut down",
                "Alt+P",
                "system-shutdown-symbolic",
                ["cosmic-osd", "shutdown"],
                destructive=True,
            ),
        ]
        for button in buttons:
            actions.append(button)

        hint = Gtk.Label(
            label="Arrow keys + Enter   ·   Alt+L/S/O/R/P direct action   ·   Esc cancel"
        )
        hint.set_halign(Gtk.Align.CENTER)
        hint.add_css_class("session-hint")
        card.append(hint)

        window.set_child(outer)
        window.present()
        buttons[0].grab_focus()

    def action_button(
        self,
        label: str,
        shortcut: str,
        icon_name: str,
        command: list[str],
        *,
        destructive: bool = False,
    ) -> Gtk.Button:
        button = Gtk.Button()
        button.add_css_class("session-action")
        if destructive:
            button.add_css_class("destructive-action")
        button.set_tooltip_text(f"{label} ({shortcut})")
        button.connect("clicked", lambda _button: self.run_action(command))

        content = Gtk.Box(orientation=Gtk.Orientation.VERTICAL, spacing=10)
        content.set_halign(Gtk.Align.CENTER)
        content.set_valign(Gtk.Align.CENTER)

        icon = Gtk.Image.new_from_icon_name(icon_name)
        icon.set_pixel_size(34)
        content.append(icon)

        text = Gtk.Label(label=label)
        text.add_css_class("action-label")
        content.append(text)

        key = Gtk.Label(label=shortcut)
        key.add_css_class("action-key")
        content.append(key)

        button.set_child(content)
        return button

    def run_action(self, command: list[str]) -> None:
        if self.window is not None:
            self.window.set_visible(False)
        subprocess.Popen(command, start_new_session=True)
        self.quit()

    def on_key_pressed(
        self,
        _controller: Gtk.EventControllerKey,
        keyval: int,
        _keycode: int,
        _state: Gdk.ModifierType,
    ) -> bool:
        if keyval == Gdk.KEY_Escape:
            if self.window is not None:
                self.window.close()
            self.quit()
            return True

        if not (_state & Gdk.ModifierType.ALT_MASK):
            return False

        name = (Gdk.keyval_name(keyval) or "").lower()
        shortcuts = {
            "l": ["loginctl", "lock-session"],
            "s": ["systemctl", "suspend"],
            "o": ["cosmic-osd", "log-out"],
            "r": ["cosmic-osd", "restart"],
            "p": ["cosmic-osd", "shutdown"],
        }
        if name in shortcuts:
            self.run_action(shortcuts[name])
            return True
        return False

    def on_close_request(self, _window: Gtk.ApplicationWindow) -> bool:
        self.window = None
        self.quit()
        return False

    @staticmethod
    def install_css(colors: dict[str, str]) -> None:
        css = f"""
        .session-overlay {{
            background-color: {colors['overlay']};
        }}

        .session-card {{
            background-color: {colors['card']};
            color: {colors['text']};
            border-radius: 20px;
            padding: 30px;
            box-shadow: 0 18px 60px rgba(0, 0, 0, 0.35);
            min-width: 790px;
        }}

        .session-title {{
            color: {colors['text']};
            font-size: 24px;
            font-weight: 700;
        }}

        .session-subtitle, .session-hint {{
            color: {colors['muted']};
        }}

        .session-subtitle {{
            font-size: 13px;
        }}

        .session-hint {{
            font-size: 11px;
        }}

        button.session-action {{
            background: {colors['button']};
            color: {colors['text']};
            border: 1px solid transparent;
            border-radius: 14px;
            box-shadow: none;
            min-width: 126px;
            min-height: 126px;
            padding: 16px 10px;
            transition: 120ms ease;
        }}

        button.session-action:hover {{
            background: {colors['button_hover']};
        }}

        button.session-action:focus-visible {{
            border-color: {colors['accent']};
            outline: 2px solid {colors['accent']};
            outline-offset: 2px;
        }}

        button.session-action.destructive-action {{
            background: {colors['destructive']};
            color: {colors['destructive_on']};
        }}

        button.session-action.destructive-action label,
        button.session-action.destructive-action image {{
            color: {colors['destructive_on']};
        }}

        .action-label {{
            font-size: 14px;
            font-weight: 600;
        }}

        .action-key {{
            color: {colors['muted']};
            font-size: 10px;
        }}

        button.destructive-action .action-key {{
            color: {colors['destructive_on']};
            opacity: 0.70;
        }}
        """

        provider = Gtk.CssProvider()
        provider.load_from_data(css.encode())
        display = Gdk.Display.get_default()
        if display is not None:
            Gtk.StyleContext.add_provider_for_display(
                display,
                provider,
                Gtk.STYLE_PROVIDER_PRIORITY_APPLICATION,
            )


if __name__ == "__main__":
    raise SystemExit(SessionMenu().run(None))
