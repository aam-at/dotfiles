#!/usr/bin/env bash

export THYME_STORAGE_PATH=~/Dropbox/Sync

while true; do
    thyme track -o $THYME_STORAGE_PATH/thyme.json;
    sleep 30s;
done;
