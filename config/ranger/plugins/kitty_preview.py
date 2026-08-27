import os
import shutil
import subprocess
import tempfile

from ranger.ext.img_display import (
    ImageDisplayer,
    ImageDisplayError,
    ImgDisplayUnsupportedException,
    register_image_displayer,
)


DEFAULT_PREVIEW_SIZE = os.environ.get("RNGR_PREVIEW_MAX_SIZE", "1920x1080")
FAST_PREVIEW_SIZE = os.environ.get("RNGR_FAST_IMAGE_PREVIEW_MAX_SIZE", "1200x1200")
IMAGE_ZOOM = 1.0
MIN_IMAGE_ZOOM = 0.25
MAX_IMAGE_ZOOM = 4.0
FAST_PREVIEW_DIRS = tuple(
    os.path.realpath(os.path.expanduser(p))
    for p in os.environ.get(
        "RNGR_FAST_IMAGE_PREVIEW_DIRS", "~/Pictures/Wallpapers"
    ).split(os.pathsep)
    if p
)


def _is_in_fast_dir(path):
    abs_path = os.path.realpath(path)
    for root in FAST_PREVIEW_DIRS:
        try:
            if os.path.commonpath([abs_path, root]) == root:
                return True
        except ValueError:
            continue
    return False


def _split_size(size):
    try:
        width, height = size.lower().split("x", 1)
        return max(1, int(width)), max(1, int(height))
    except (AttributeError, TypeError, ValueError):
        return 1920, 1080


def _format_size(size):
    width, height = _split_size(size)
    return f"{width}x{height}"


def _term_name():
    return " ".join(
        value.lower()
        for value in (
            os.environ.get("TERM", ""),
            os.environ.get("TERM_PROGRAM", ""),
        )
    )


@register_image_displayer("kitty")
class KittyImageDisplayer(ImageDisplayer):
    def __init__(self):
        self.kitty_bin = shutil.which("kitty")
        self.wezterm_bin = shutil.which("wezterm")

    def _helper(self):
        term = _term_name()
        if self.wezterm_bin and "wezterm" in term:
            return "wezterm"
        if self.kitty_bin and (
            "kitty" in term or "ghostty" in term or os.environ.get("KITTY_WINDOW_ID")
        ):
            return "kitty"
        raise ImgDisplayUnsupportedException(
            "image previews require Kitty or WezTerm graphics support"
        )

    def _preview_size(self, path):
        size = FAST_PREVIEW_SIZE if _is_in_fast_dir(path) else DEFAULT_PREVIEW_SIZE
        return _format_size(size)

    def _zoomed_dims(self, width, height):
        zoom = max(MIN_IMAGE_ZOOM, min(MAX_IMAGE_ZOOM, float(IMAGE_ZOOM)))
        return max(1, int(round(width * zoom))), max(1, int(round(height * zoom)))

    def _render_pdf(self, path):
        pdftoppm = shutil.which("pdftoppm")
        if pdftoppm is None:
            raise ImageDisplayError("pdftoppm not found in PATH")

        base_width, base_height = _split_size(self._preview_size(path))
        render_width, _ = self._zoomed_dims(base_width, base_height)

        fd, prefix = tempfile.mkstemp(prefix="ranger-pdf-")
        os.close(fd)
        os.unlink(prefix)
        rendered = f"{prefix}.png"

        try:
            subprocess.run(
                [
                    pdftoppm,
                    "-f",
                    "1",
                    "-l",
                    "1",
                    "-singlefile",
                    "-scale-to-x",
                    str(render_width),
                    "-scale-to-y",
                    "-1",
                    "-png",
                    "--",
                    path,
                    prefix,
                ],
                check=True,
                stderr=subprocess.DEVNULL,
            )
            return rendered
        except (OSError, subprocess.CalledProcessError) as exc:
            raise ImageDisplayError(str(exc))

    def draw(self, path, start_x, start_y, width, height):
        helper = self._helper()
        zoom_width, zoom_height = self._zoomed_dims(width, height)
        render_path = path
        cleanup_path = None

        if path.lower().endswith(".pdf"):
            render_path = self._render_pdf(path)
            cleanup_path = render_path

        if helper == "kitty":
            cmd = [
                self.kitty_bin,
                "+kitten",
                "icat",
                "--place",
                f"{zoom_width}x{zoom_height}@{start_x}x{start_y}",
                "--scale-up",
                "--no-trailing-newline",
                render_path,
            ]
        else:
            cmd = [
                self.wezterm_bin,
                "imgcat",
                "--width",
                str(zoom_width),
                "--height",
                str(zoom_height),
                "--position",
                f"{start_x},{start_y}",
                "--resize",
                self._preview_size(path),
                "--no-move-cursor",
                render_path,
            ]

        try:
            subprocess.run(
                cmd,
                check=True,
                stderr=subprocess.DEVNULL,
            )
        except (OSError, subprocess.CalledProcessError) as exc:
            raise ImageDisplayError(str(exc))
        finally:
            if cleanup_path:
                try:
                    os.unlink(cleanup_path)
                except OSError:
                    pass

    def clear(self, start_x, start_y, width, height):
        try:
            helper = self._helper()
        except ImgDisplayUnsupportedException:
            return

        if helper == "kitty":
            try:
                subprocess.run(
                    [self.kitty_bin, "+kitten", "icat", "--clear", "--silent"],
                    check=True,
                    stderr=subprocess.DEVNULL,
                )
            except (OSError, subprocess.CalledProcessError):
                pass

    def quit(self):
        pass
