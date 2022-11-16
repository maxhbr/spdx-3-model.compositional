#!/usr/bin/env nix-shell
#! nix-shell -i bash -p stack jq exiftool cloc
# shellcheck shell=bash disable=SC1008

# SPDX-FileCopyrightText: Maximilian Huber <oss@maximilian-huber.de>
#
# SPDX-License-Identifier: MIT

set -euo pipefail


root="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
stackyaml="$root/stack.yaml"


spdx3model-exe() {
    >&2 echo "build:..."
    stack --stack-yaml "$stackyaml"\
        build

    >&2 echo "run:..."
    time exec stack --stack-yaml "$stackyaml" \
        exec -- spdx3model-exe \
        "$@"
}

spdx3model-exe "$@"
