#!/bin/sh
# Usage: ./sh/update-news.sh

set -eu

REPO_SLUG="canmod/macpan2"
NARRATIVES="news-narratives.md"
BUMP_FILE="version-bumps.txt"
NEWS_FILE="NEWS.md"
TMP_FILE=$(mktemp)

# Read version bumps into an array (latest last), then reverse it
awk '{ print NR, $0 }' "$BUMP_FILE" | sort -rn | cut -d' ' -f2- | while read -r version hash date; do
  if [ "$version" != "0.0.0.9000" ]; then

    prev_hash=$(awk -v v="$version" '
      $1 == v { print prev; exit }
      { prev = $2 }
      ' "$BUMP_FILE")
    
    next_hash=$(awk -v v="$version" '
        BEGIN { found=0 }
        $1 == v { found=1; next }
        found { print $2; exit }
      ' "$BUMP_FILE"
    )
  
    echo "## Changes in $version" >> "$TMP_FILE"
    echo "" >> "$TMP_FILE"
    if [ -n "$next_hash" ]; then
      echo "Released: $date" >> "$TMP_FILE"
    else
      echo "In-Progress" >> "$TMP_FILE"
    fi
    echo "" >> "$TMP_FILE"
    echo "[change list](https://github.com/$REPO_SLUG/compare/$prev_hash..$hash)" >> "$TMP_FILE"
  
    awk "/^## $version\$/{flag=1;next}/^## /{flag=0}flag" "$NARRATIVES" >> "$TMP_FILE"
    echo "" >> "$TMP_FILE"
  
  fi
done

mv "$TMP_FILE" "$NEWS_FILE"
