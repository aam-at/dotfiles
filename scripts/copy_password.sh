#!/usr/bin/env sh

if [ $# -eq 0 ]
then
    echo "No arguments supplied"
    exit 1;
fi

login="$1"

echo $(gpg2 -q --for-your-eyes-only --no-tty -d ~/.authinfo.gpg | awk '/login[ ]+'$1' / {print $NF}')
