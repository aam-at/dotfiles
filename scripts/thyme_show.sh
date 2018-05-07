#!/usr/bin/env bash

export THYME_STORAGE_PATH=~/Dropbox/Sync

thyme show -i $THYME_STORAGE_PATH/thyme.json -w stats > $THYME_STORAGE_PATH/thyme.html
