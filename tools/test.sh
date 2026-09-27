#!/usr/bin/env sh
# Checks the C tools under tools/ (the shared library in tools/lib on the
# include path): each <name>.c compiles with warnings as errors, platform code
# included, and the <name>.test.c beside it (which includes it) builds and
# passes. On Linux, and on Windows in Git Bash with MinGW gcc; the pre-commit
# hook runs it.
set -eu

root=$(cd "$(dirname "$0")" && pwd)
case "$(uname -s)" in
MINGW* | MSYS* | CYGWIN* | Windows_NT)
  # -Wextra flags standard Win32 idioms ({sizeof x} initializers, callbacks).
  flags="-Wall -Werror" libs="-lws2_32 -ldwmapi" ext=.exe
  ;;
*)
  # Display strings are truncated on purpose when they don't fit.
  flags="-Wall -Wextra -Wno-format-truncation -Werror" libs="-lm" ext=
  ;;
esac
cc=${CC:-cc}
command -v "$cc" >/dev/null 2>&1 || cc=gcc

out=$(mktemp -d)
trap 'rm -rf "$out"' EXIT
status=0
for source in $(find "$root" -name '*.c' ! -name '*.test.c' | sort); do
  name=$(basename "$source" .c)
  # shellcheck disable=SC2086 # word-split flags and libs
  $cc $flags -I "$root/lib" -c -o "$out/$name.o" "$source" || status=1
  test="${source%.c}.test.c"
  [ -f "$test" ] || continue
  # shellcheck disable=SC2086
  if $cc $flags -I "$root/lib" -o "$out/$name.test$ext" "$test" $libs; then
    "$out/$name.test$ext" || status=1
  else
    status=1
  fi
done
exit $status
