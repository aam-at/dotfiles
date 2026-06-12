import os
import ranger.api
import shutil
import subprocess
from ranger.api.commands import *

HOOK_INIT_OLD = ranger.api.hook_init
AUTOJUMP = shutil.which("autojump")


def _resolve_directory(path):
    if not path:
        return None

    expanded = os.path.expandvars(os.path.expanduser(path))
    if os.path.isdir(expanded):
        return os.path.abspath(expanded)

    return None


def hook_init(fm):
    if AUTOJUMP:
        def update_autojump(signal):
            try:
                subprocess.Popen([AUTOJUMP, "--add", signal.new.path])
            except OSError:
                pass

        fm.signal_bind('cd', update_autojump)
    HOOK_INIT_OLD(fm)


ranger.api.hook_init = hook_init


class j(Command):
    """:j

    Uses autojump to set the current directory.
    """

    def execute(self):
        query = self.arg(1)
        directory = _resolve_directory(query)

        if not query:
            if directory:
                self.fm.cd(directory)
            else:
                self.fm.notify("autojump is not available", bad=True)
            return

        if AUTOJUMP:
            try:
                directory = subprocess.check_output([AUTOJUMP, query])
                directory = directory.decode("utf-8", "ignore").rstrip("\n")
            except (OSError, subprocess.CalledProcessError):
                if directory:
                    self.fm.cd(directory)
                else:
                    self.fm.notify("autojump is not available", bad=True)
                return
        elif not directory:
            self.fm.notify("autojump is not available", bad=True)
            return

        if directory:
            self.fm.cd(directory)
