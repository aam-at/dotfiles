#!/usr/bin/env bash

set -euo pipefail

if ! command -v go >/dev/null 2>&1; then
  echo "go is not installed. Install Go before running this script." >&2
  exit 1
fi

go_repos=(
  "charmbracelet/freeze"
  "charmbracelet/glow"
  "charmbracelet/mods"
  "charmbracelet/vhs"
  "dundee/gdu"
  "jesseduffield/lazydocker"
  "stefanlogue/meteor"
)

for repo in "${go_repos[@]}"; do
  go install "github.com/${repo}@latest"
done
