#!/bin/sh
#
# This file is part of AtomVM.
#
# Copyright 2023 Fred Dushin <fred@dushin.net>
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#    http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#
# SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
#

#
# Install script for the packbeam utility.
#
# This script will install the packbeam utility in a given location
# on the user's machine.  You must provide the prefix location for the
# installation, in addition to the currently operative version, as
# defined in the relx section of this project's rebar.config.
#
# Syntax: install.sh <prefix> <version>
#     where <prefix> is the prefix location for the install
#           <version> is the current release packbeam version
#
# This script will create a self-contained packbeam installation, including
# ERTS as built for the target platform.  Effort is made to not conflict
# with any other ERTS or Erlang installations already on the machine.
#
# After installation, users may run <prefix>/bin/packbeam on the
# command line.  (Some installation tools will provide this automatically
# in the user's PATH)
#
# Set the environemnt variable PACKBEAM_DEBUG to a non-empty string
# to get diagnostic information about the installation.
#

set -e

readonly root_dir="$(cd $(dirname $0) && pwd)"

readonly nargs=$#
if [[ ${nargs} -lt 2 ]]; then
    echo
    echo "Syntax: $0 <prefix> <version>"
    echo "    where <prefix> is the prefix location for the install"
    echo "          <version> is the current release packbeam version"
    echo
    exit 1
fi
readonly prefix="${1}"
readonly version="${2}"

if [ ! -e "${prefix}" ]; then
    echo "ERROR! Prefix dir ${prefix} must exist!"
    exit 1
fi

echo_run() {
    local cmd="$@"
    if [ -n "${PACKBEAM_DEBUG}" ]; then
        echo "# $(date) [$(hostname)]> ${cmd}"
    fi
    ${cmd}
}

readonly src_tar="${root_dir}/_build/default/rel/atomvm_packbeam/atomvm_packbeam-${version}.tar.gz"
if [ ! -e "${src_tar}" ]; then
    echo "ERROR! It looks like atomvm_packbeam version ${version} has not been built!"
    exit 1
fi

## unzip the archive (so that BSD tar can deal with it)
readonly tmp_dir="$(mktemp -d /tmp/atomvm_packbeam.XXXXXX)"
echo_run cp "${src_tar}" "${tmp_dir}/."
echo_run gunzip "${tmp_dir}/atomvm_packbeam-${version}.tar.gz"

readonly dest_dir="${prefix}/atomvm_packbeam"
if [ -e "${dest_dir}" ]; then
    echo "ERROR! It looks like ${dest_dir} already exists!"
    exit 1
fi

echo_run mkdir -p "${dest_dir}"
echo_run tar -C "${dest_dir}" -x -f "${tmp_dir}/atomvm_packbeam-${version}.tar"

echo_run mkdir -p "${prefix}/bin"
echo_run mv "${dest_dir}/bin/packbeam.sh" "${prefix}/bin/packbeam"
echo_run chmod 755 "${prefix}/bin/packbeam"

echo_run rm -rf "${tmp_dir}"
echo "atomvm_packbeam version ${version} installed in ${dest_dir}."
