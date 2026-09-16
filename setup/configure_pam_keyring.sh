#!/usr/bin/env bash
# Enable GNOME Keyring unlock through the display-manager PAM stack.

set -euo pipefail

readonly GREETD_PAM=/etc/pam.d/greetd
readonly PASSWD_PAM=/etc/pam.d/passwd
readonly KEYRING_MODULE=/usr/lib/security/pam_gnome_keyring.so

usage() {
  cat <<'EOF'
Usage: configure_pam_keyring.sh [--check|--apply]

--check  Report whether greetd and password-change PAM hooks are present.
--apply  Back up and add the hooks (the default; uses sudo when needed).

This unlocks the GNOME Login keyring, including its SSH-agent component,
with the account password. GnuPG private-key caching remains managed by
gpg-agent; pam_gnome_keyring does not unlock GnuPG key passphrases.
EOF
}

check_configuration() {
  local failed=0

  [[ -r $KEYRING_MODULE ]] || {
    printf 'Missing PAM module: %s\n' "$KEYRING_MODULE" >&2
    failed=1
  }

  if ! grep -Eq '^[[:space:]]*-?auth[[:space:]]+optional[[:space:]]+pam_gnome_keyring\.so([[:space:]]|$)' "$GREETD_PAM"; then
    printf 'Missing greetd auth hook: %s\n' "$GREETD_PAM" >&2
    failed=1
  fi
  if ! grep -Eq '^[[:space:]]*-?session[[:space:]]+optional[[:space:]]+pam_gnome_keyring\.so[[:space:]]+auto_start([[:space:]]|$)' "$GREETD_PAM"; then
    printf 'Missing greetd session hook: %s\n' "$GREETD_PAM" >&2
    failed=1
  fi
  if ! grep -Eq '^[[:space:]]*-?password[[:space:]]+optional[[:space:]]+pam_gnome_keyring\.so([[:space:]]|$)' "$PASSWD_PAM"; then
    printf 'Missing password-sync hook: %s\n' "$PASSWD_PAM" >&2
    failed=1
  fi

  return "$failed"
}

insert_auth_hook() {
  awk '
    /^[[:space:]]*-?auth[[:space:]]+optional[[:space:]]+pam_gnome_keyring\.so([[:space:]]|$)/ { found=1 }
    /^[[:space:]]*account[[:space:]]/ && !found {
      print "auth       optional     pam_gnome_keyring.so"
      found=1
    }
    { print }
    END { if (!found) exit 2 }
  ' "$1"
}

insert_session_hook() {
  local file=$1
  if grep -Eq '^[[:space:]]*-?session[[:space:]]+optional[[:space:]]+pam_gnome_keyring\.so[[:space:]]+auto_start([[:space:]]|$)' "$file"; then
    cat "$file"
  else
    cat "$file"
    printf 'session    optional     pam_gnome_keyring.so auto_start\n'
  fi
}

insert_password_hook() {
  local file=$1
  if grep -Eq '^[[:space:]]*-?password[[:space:]]+optional[[:space:]]+pam_gnome_keyring\.so([[:space:]]|$)' "$file"; then
    cat "$file"
  else
    cat "$file"
    printf 'password   optional     pam_gnome_keyring.so\n'
  fi
}

install_if_changed() {
  local source=$1
  local destination=$2
  local mode backup

  if cmp -s "$source" "$destination"; then
    return 0
  fi
  mode=$(stat -c '%a' "$destination")
  backup="$destination.bak.$backup_suffix"
  sudo cp -a "$destination" "$backup"
  sudo install -o root -g root -m "$mode" "$source" "$destination.new"
  sudo mv "$destination.new" "$destination"
  printf 'Updated %s (backup: %s)\n' "$destination" "$backup"
}

mode=apply
case ${1:-} in
'') ;;
--check) mode=check ;;
--apply) mode=apply ;;
-h | --help | help)
  usage
  exit 0
  ;;
*)
  usage >&2
  exit 2
  ;;
esac

if [[ $mode == check ]]; then
  check_configuration
  printf 'PAM keyring hooks are configured.\n'
  exit 0
fi

if [[ ! -r $GREETD_PAM || ! -r $PASSWD_PAM ]]; then
  printf 'Expected PAM files are missing: %s and/or %s\n' "$GREETD_PAM" "$PASSWD_PAM" >&2
  exit 1
fi
[[ -r $KEYRING_MODULE ]] || {
  printf 'Install gnome-keyring before running this script.\n' >&2
  exit 1
}
command -v sudo >/dev/null || {
  printf 'sudo is required to modify system PAM files.\n' >&2
  exit 1
}

backup_suffix=$(date +%Y%m%d-%H%M%S)
temporary_directory=$(mktemp -d)
trap 'rm -rf "$temporary_directory"' EXIT

insert_auth_hook "$GREETD_PAM" >"$temporary_directory/greetd.auth"
insert_session_hook "$temporary_directory/greetd.auth" >"$temporary_directory/greetd"
insert_password_hook "$PASSWD_PAM" >"$temporary_directory/passwd"

install_if_changed "$temporary_directory/greetd" "$GREETD_PAM"
install_if_changed "$temporary_directory/passwd" "$PASSWD_PAM"

printf '\nReview with: sudo nl -ba %s %s\n' "$GREETD_PAM" "$PASSWD_PAM"
printf 'Log out and back in for PAM to unlock the Login keyring.\n'
