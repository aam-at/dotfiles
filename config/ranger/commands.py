import os
import shlex
import shutil
import subprocess

from ranger.api.commands import *
from ranger.core.loader import CommandLoader


class emptytrash(Command):
    """:empty

    Empties the trash
    """

    def execute(self):
        HOME = os.environ['HOME']
        self.fm.run(f'trash-empty')


class extracthere(Command):
    def execute(self):
        """ Extract copied files to current directory """
        copied_files = tuple(self.fm.copy_buffer)

        if not copied_files:
            return

        def refresh(_):
            cwd = self.fm.get_directory(original_path)
            cwd.load_content()

        one_file = copied_files[0]
        cwd = self.fm.thisdir
        original_path = cwd.path
        au_flags = ['-X', cwd.path]
        au_flags += self.line.split()[1:]
        au_flags += ['-e']

        self.fm.copy_buffer.clear()
        self.fm.cut_buffer = False
        if len(copied_files) == 1:
            descr = "extracting: " + os.path.basename(one_file.path)
        else:
            descr = "extracting files from: " + os.path.basename(one_file.dirname)
        obj = CommandLoader(args=['aunpack'] + au_flags \
                + [f.path for f in copied_files], descr=descr)

        obj.signal_bind('after', refresh)
        self.fm.loader.add(obj)


class lazygit(Command):
    """:lazygit

    Launch lazygit rooted at the closest git repository (or cwd).
    """

    def execute(self):
        if shutil.which("lazygit") is None:
            self.fm.notify("lazygit not found in PATH", bad=True)
            return

        target = self._git_root(self.fm.thisdir.path) or self.fm.thisdir.path
        self.fm.run(f"lazygit -p {shlex.quote(target)}")

    def _git_root(self, directory):
        if shutil.which("git") is None:
            return None

        process = self.fm.execute_command(
            ["git", "-C", directory, "rev-parse", "--show-toplevel"],
            stdout=subprocess.PIPE,
            stderr=subprocess.DEVNULL,
            universal_newlines=True,
        )
        stdout, _ = process.communicate()
        if process.returncode != 0:
            return None
        return stdout.strip()


class fd_select(Command):
    """
    :fd_select

    Use fd + fzf (with previews) to quickly jump to files or directories.
    Hold a count prefix to limit search to directories only.
    """

    def execute(self):
        if shutil.which("fd") is None:
            self.fm.notify("fd (https://github.com/sharkdp/fd) is required", bad=True)
            return
        if shutil.which("fzf") is None:
            self.fm.notify("fzf is required for fd_select", bad=True)
            return

        fd_cmd = "fd --hidden --follow --exclude .git --color=always"
        if self.quantifier:
            fd_cmd += " --type directory"

        preview_cmd = (
            "bat --style=numbers --color=always {} 2>/dev/null | head -200"
            if shutil.which("bat")
            else "sed -n '1,200p' {} 2>/dev/null"
        )

        command = (
            f"{fd_cmd} | fzf --ansi --preview {shlex.quote(preview_cmd)} "
            "--height 80% --layout=reverse --border"
        )

        finder = self.fm.execute_command(
            command,
            universal_newlines=True,
            stdout=subprocess.PIPE,
        )
        stdout, _ = finder.communicate()
        if finder.returncode != 0:
            return

        selection = next(
            (line.strip() for line in reversed(stdout.splitlines()) if line.strip()),
            None,
        )
        if not selection:
            return

        target = os.path.abspath(selection)
        if os.path.isdir(target):
            self.fm.cd(target)
        else:
            self.fm.select_file(target)


class ripgrep(Command):
    """
    :rg <pattern>

    Search with ripgrep + fzf; select a match to jump to the file.
    Provide a count prefix (e.g. 1rg) to open the match in $EDITOR at that line.
    """

    def execute(self):
        if shutil.which("rg") is None:
            self.fm.notify("ripgrep (rg) is required for :rg", bad=True)
            return
        if shutil.which("fzf") is None:
            self.fm.notify("fzf is required for :rg", bad=True)
            return

        pattern = self.rest(1)
        if not pattern:
            self.fm.open_console("rg ")
            return

        root = self.fm.thisdir.path
        command = self._build_command(pattern)
        finder = self.fm.execute_command(
            ["bash", "-lc", command],
            universal_newlines=True,
            stdout=subprocess.PIPE,
        )
        stdout, _ = finder.communicate()
        if finder.returncode != 0:
            return

        selection = stdout.strip()
        if not selection:
            return

        file_path, line_no = self._parse_selection(selection)
        if not file_path:
            return

        abs_path = (
            file_path if os.path.isabs(file_path) else os.path.join(root, file_path)
        )
        abs_path = os.path.abspath(abs_path)

        if not os.path.exists(abs_path):
            self.fm.notify(f"No such file: {abs_path}", bad=True)
            return

        self.fm.select_file(abs_path)
        if line_no:
            rel = os.path.relpath(abs_path, root)
            self.fm.notify(f"rg → {rel}:{line_no}")
            if self.quantifier:
                self._open_in_editor(abs_path, line_no)

    def _build_command(self, pattern):
        preview = (
            "bat --style=numbers --color=always --highlight-line {2} {1}"
            if shutil.which("bat")
            else "sed -n '{2}p' {1}"
        )
        preview_quoted = shlex.quote(preview)
        rg_cmd = (
            "rg --hidden --glob '!.git' --smart-case --line-number --column "
            "--no-heading --color=always --"
        )
        quoted_pattern = shlex.quote(pattern)
        return (
            f"{rg_cmd} {quoted_pattern} | "
            f"fzf --ansi --delimiter ':' "
            f"--preview {preview_quoted} "
            "--preview-window 'down,60%,border'"
        )

    def _parse_selection(self, line):
        parts = line.split(":", 3)
        if not parts:
            return None, None
        path = parts[0]
        try:
            line_no = int(parts[1]) if len(parts) > 1 else None
        except ValueError:
            line_no = None
        return path, line_no

    def _open_in_editor(self, path, line_no):
        editor = os.environ.get("VISUAL") or os.environ.get("EDITOR")
        if not editor:
            self.fm.notify("Set $EDITOR to open matches directly", bad=True)
            return
        command = f"{editor} +{line_no} {shlex.quote(path)}"
        self.fm.run(command)
