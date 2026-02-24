import os.path
import shutil
import subprocess

from ranger.api.commands import *


class fzf_select(Command):
    """
    :fzf_select

    Find a file using fzf.

    With a prefix argument select only directories.

    See: https://github.com/junegunn/fzf
    """
    def _build_command(self):
        fd_cmd = shutil.which("fd") or shutil.which("fdfind")
        if fd_cmd:
            if self.quantifier:
                # match only directories
                return "{} --follow --no-ignore --type d . 2> /dev/null | fzf +m".format(fd_cmd)

            # match files and directories
            return "{} --follow --no-ignore . 2> /dev/null | fzf +m".format(fd_cmd)

        if self.quantifier:
            # match only directories
            return "find -L . \\( -path '*/\\.*' -o -fstype 'dev' -o -fstype 'proc' \\) -prune \
            -o -type d -print 2> /dev/null | sed 1d | cut -b3- | fzf +m"

        # match files and directories
        return "find -L . \\( -path '*/\\.*' -o -fstype 'dev' -o -fstype 'proc' \\) -prune \
            -o -print 2> /dev/null | sed 1d | cut -b3- | fzf +m"

    def execute(self):
        command = self._build_command()

        fzf = self.fm.execute_command(command,
                                      universal_newlines=True,
                                      stdout=subprocess.PIPE)
        stdout, stderr = fzf.communicate()
        if fzf.returncode == 0:
            fzf_file = os.path.abspath(stdout.rstrip('\n'))
            if os.path.isdir(fzf_file):
                self.fm.cd(fzf_file)
            else:
                self.fm.select_file(fzf_file)
