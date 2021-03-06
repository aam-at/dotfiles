#!/usr/bin/env python
# -*- coding: utf-8 -*-
import os
from subprocess import call


def walklevel(some_dir, level=1):
    some_dir = some_dir.rstrip(os.path.sep)
    assert os.path.isdir(some_dir)
    num_sep = some_dir.count(os.path.sep)
    for root, dirs, files in os.walk(some_dir):
        yield root, dirs, files
        num_sep_this = root.count(os.path.sep)
        if num_sep + level <= num_sep_this:
            del dirs[:]

join, isfile, isdir = os.path.join, os.path.isfile, os.path.isdir
for dir_path, sub_dirs, files in walklevel('.', level=2):
    if ".git" in sub_dirs:
        print("Pulling changes for %s" % dir_path)
        # in case if there are unstaged changes
        call(["git", "stash"], cwd=dir_path)
        call(["git", "pull", "--rebase"], cwd=dir_path)
        call(["git", "stash", "pop"])
    if "setup.py" in files:
        setup_arg = 'develop'
        if dir_path == 'fuel':
            setup_arg = 'install'
        else:
            setup_arg = 'develop'
        print("Setup.py for %s" % dir_path)
        call(["python", "setup.py", setup_arg], cwd=dir_path)
