#!/bin/bash
# Usage: ./sh/update-version-bumps.sh

set -euo pipefail

MAP_FILE=commit-version-map.txt
BUMP_FILE=version-bumps.txt

# space-separated list of versions to exclude
EXCLUDE_VERSIONS="2.2.1 2.2.3 2.5.0"

awk -v exclude="$EXCLUDE_VERSIONS" '
BEGIN {
  n = split(exclude, skiplist)
  for (i = 1; i <= n; i++) {
    skip[skiplist[i]] = 1
  }
}
{
  version = $1
  if (!(version in skip)) {
    lines[version] = $0
  }
}
END {
  for (v in lines) {
    print lines[v]
  }
}
' "$MAP_FILE" | sort -k1,1V > "$BUMP_FILE"
