#!/bin/bash
# Usage: ./sh/update-version-bumps.sh

set -euo pipefail

MAP_FILE=commit-version-map.txt
BUMP_FILE=version-bumps.txt

awk '
{
  version = $1
  lines[version] = $0
}
END {
  for (v in lines) {
    print lines[v]
  }
}
' "$MAP_FILE" | sort -k3,3 > "$BUMP_FILE"
