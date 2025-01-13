#!/usr/bin/env bash

# Dropdown Terminal Script using hdrop for hyperland

# Check if tdrop is installed
if ! command -v hdrop &>/dev/null; then
  echo "hrdrop could not be found. Please install hdrop (https://github.com/Schweber/hdrop)."
  exit 1
fi

# Check if kitty is installed
if ! command -v kitty &>/dev/null; then
  echo "kitty could not be found. Please install kitty."
  exit 1
fi

# Set default values for width and height
WIDTH="100"
HEIGHT="50"
CLASS=kitty-dropterm

hdrop --floating --position top --gap 40 --width $WIDTH --height $HEIGHT kitty --class $CLASS
