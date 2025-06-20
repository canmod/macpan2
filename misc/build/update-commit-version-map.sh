#!/bin/bash
# Usage: ./sh/update-commit-version-map.sh

set -euo pipefail

MAP_FILE=commit-version-map.txt
TMP_FILE=$(mktemp)

last_hash=$(tail -n 1 "$MAP_FILE" 2>/dev/null | awk '{print $2}')

if [[ -n "$last_hash" ]]; then
  new_commits=$(git log main --pretty=format:"%H %ad" --date=short --reverse "$last_hash"..main)
else
  new_commits=$(git log main --pretty=format:"%H %ad" --date=short --reverse)
fi

# Process new commits
while IFS= read -r line; do
  commit_hash=$(echo "$line" | awk '{print $1}')
  commit_date=$(echo "$line" | awk '{print $2}')
  version=$(git show "$commit_hash:DESCRIPTION" 2>/dev/null | awk -F': ' '/^Version:/ {print $2}')
  if [[ -n "$version" && -n "$commit_hash" && -n "$commit_date" ]]; then
    echo "$version $commit_hash $commit_date" >> "$TMP_FILE"
  fi
done <<< "$new_commits"

cat "$TMP_FILE" >> "$MAP_FILE"
rm "$TMP_FILE"


