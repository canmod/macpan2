#!/bin/bash

set -euo pipefail

commit="$1"
results_file="install-tests.txt"

# Exit if working directory is dirty (unstaged or staged changes)
if ! git diff --quiet || ! git diff --cached --quiet; then
  echo "ERROR: Working directory has uncommitted changes. Stash or commit first."
  exit 1
fi

# Save current ref
current_ref=$(git rev-parse --abbrev-ref HEAD)
[[ "$current_ref" == "HEAD" ]] && current_ref=$(git rev-parse HEAD)

# Checkout the version to test
if ! git checkout --quiet "$commit"; then
  echo "$commit CHECKOUT-FAIL"
  echo "$commit CHECKOUT-FAIL" >> "$results_file"
  exit 0
fi

# Run the install and capture result
if make quick-doc-install; then
  result="OK"
else
  result="FAIL"
fi

# Restore any changed files from this test version
git restore .

# Checkout original ref (should now succeed)
git checkout --quiet "$current_ref"

# Record result, overwriting old entry if it exists
tmpfile=$(mktemp)
grep -v "^$commit " "$results_file" 2>/dev/null > "$tmpfile" || true
echo "$commit $result" >> "$tmpfile"
mv "$tmpfile" "$results_file"

echo "$commit $result"
