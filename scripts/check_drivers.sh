#!/bin/sh
# Copyright 2026 Paul Guyot <pguyot@kallisys.net>
# SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later

# Checks that src/packbeam_drivers.hrl is what scripts/generate_drivers.escript
# produces from the latest AtomVM release tag, pre-releases included.
#
#     scripts/check_drivers.sh            # fail if the file is out of date
#     scripts/check_drivers.sh --update   # regenerate it instead
#
# ATOMVM_REPO overrides the AtomVM repository to clone.

set -eu

cd "$(dirname "$0")/.."

REPO=${ATOMVM_REPO:-https://github.com/atomvm/AtomVM.git}
DRIVERS=src/packbeam_drivers.hrl

# With the `-' suffix, v0.7.0-beta.0 sorts before v0.7.0.
TAG=$(git -c versionsort.suffix=- ls-remote --tags --refs --sort=-v:refname "$REPO" 'v*' \
    | sed -n 's|^.*refs/tags/||p' \
    | grep -E '^v[0-9]+\.[0-9]+\.[0-9]+(-[a-z]+\.[0-9]+)?$' \
    | head -n 1)
if [ -z "$TAG" ]; then
    echo "No AtomVM release tag found in $REPO" >&2
    exit 1
fi
echo "Latest AtomVM release: $TAG"

WORK=$(mktemp -d)
trap 'rm -rf "$WORK"' EXIT
git -c advice.detachedHead=false clone --quiet --depth 1 --branch "$TAG" "$REPO" "$WORK/AtomVM"

if [ "${1:-}" = "--update" ]; then
    escript scripts/generate_drivers.escript "$WORK/AtomVM" "$DRIVERS"
    exit 0
fi

escript scripts/generate_drivers.escript "$WORK/AtomVM" "$WORK/packbeam_drivers.hrl"
if ! diff -u "$DRIVERS" "$WORK/packbeam_drivers.hrl"; then
    echo "::error file=$DRIVERS::$DRIVERS does not match AtomVM $TAG; run scripts/check_drivers.sh --update"
    exit 1
fi
echo "$DRIVERS matches AtomVM $TAG"
