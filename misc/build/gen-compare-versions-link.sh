#!/bin/bash

repo_url="https://github.com/canmod/macpan2"

commits=()
versions=()

# Gather commits and versions into a file
git log --reverse -G'^Version:' --pretty=format:'%h' -- DESCRIPTION | while read -r hash; do
  version=$(git show "$hash:DESCRIPTION" | grep '^Version:' | awk '{print $2}')
  echo "$hash $version"
done > /tmp/version_hashes.txt

# Read into arrays
while read -r hash version; do
  commits+=("$hash")
  versions+=("$version")
done < /tmp/version_hashes.txt

# Get current HEAD version
current_version=$(grep '^Version:' DESCRIPTION | awk '{print $2}')
last_index=$((${#commits[@]} - 1))
last_commit=${commits[$last_index]}

# If HEAD version is newer than last bump, add main compare
if [ "$current_version" != "${versions[$last_index]}" ]; then
  short_from=$(echo "${commits[$last_index]}" | cut -c1-7)
  short_to=$(git rev-parse --short main)
  echo "$current_version : [change list]($repo_url/compare/$short_from..$short_to)"
fi

# Print previous version changes in reverse order
for ((i=last_index; i>0; i--)); do
  from=${commits[$i-1]}
  to=${commits[$i]}
  version=${versions[$i]}
  short_from=$(echo "$from" | cut -c1-7)
  short_to=$(echo "$to" | cut -c1-7)
  echo "$version : [change list]($repo_url/compare/$short_from..$short_to)"
done

rm -f /tmp/version_hashes.txt
