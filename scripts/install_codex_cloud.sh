#!/usr/bin/env bash
set -euo pipefail

HUGO_VERSION="${HUGO_VERSION:-0.145.0}"

if command -v hugo >/dev/null 2>&1; then
  hugo version | tr -d '\r'
  exit 0
fi

mise install "hugo@${HUGO_VERSION}"
mise which hugo >/dev/null 2>&1
