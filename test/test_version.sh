#!/bin/sh
set -eu

ocaml=$1
generator=$2
temp_dir=$(mktemp -d)
trap 'rm -rf "$temp_dir"' EXIT HUP INT TERM

round_trip() {
  "$ocaml" "$generator" "$1" >"$temp_dir/version.ml"
  printf '\nlet () = print_string s\n' >>"$temp_dir/version.ml"
  actual=$("$ocaml" "$temp_dir/version.ml")
  if [ "$actual" != "$1" ]; then
    echo 'Embedded version did not round-trip' >&2
    exit 1
  fi
}

round_trip dev
round_trip v1.2.3
round_trip 'v1.2.3-"quoted"\suffix'
round_trip 'first line
second line'
