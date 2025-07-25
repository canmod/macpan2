#!/bin/bash
# Usage: ./sh/update-commit-version-map.sh

set -euo pipefail

MAP_FILE=commit-version-map.txt
TMP_FILE=$(mktemp)

touch "$MAP_FILE"  # Ensure the map file exists

# If the map file is empty, initialize it with the first entry
# This is useful for the first run or if the file was cleared.
# The initial entry is a placeholder for the first commit.
if [[ -f "$MAP_FILE" && ! -s "$MAP_FILE" ]]; then
  echo "0.0.0.9000 92df5b39663259c25e8452785e13071869b7fbed 2022-06-24" > "$MAP_FILE"
fi

# Get the last commit hash from the map file
last_hash=$(tail -n 1 "$MAP_FILE" 2>/dev/null | awk '{print $2}')
new_commits=$(git log main --pretty=format:"%H %ad" --date=short --reverse "$last_hash"..main)

# Process new commits
echo "Processing new commits since last entry in $MAP_FILE..."
while IFS= read -r line; do
  commit_hash=$(echo "$line" | awk '{print $1}')
  commit_date=$(echo "$line" | awk '{print $2}')
  version=$(git show "$commit_hash:DESCRIPTION" 2>/dev/null | awk -F': ' '/^Version:/ {print $2}')
  if [[ -n "$version" && -n "$commit_hash" && -n "$commit_date" ]]; then
    echo "$version $commit_hash $commit_date"
    echo "$version $commit_hash $commit_date" >> "$TMP_FILE"
  fi
done <<< "$new_commits"

cat "$TMP_FILE" >> "$MAP_FILE"
rm "$TMP_FILE"
