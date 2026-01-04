#!/usr/bin/env bash
set -x
set -euo pipefail

HUGO_VERSION="${HUGO_VERSION:-0.145.0}"
mise install "hugo@${HUGO_VERSION}"
mise use
