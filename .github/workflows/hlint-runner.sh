#!/usr/bin/env bash

set -eux

source .github/workflows/cpus.sh

git add .

find src test -name "*.hs" | parallel -j "$CPUS" -- hlint --refactor-options="-i" --refactor {}

git status

set +e

git diff --exit-code
diff_code=$?

if [ $diff_code -ne 0 ]
then
  echo "Test Hlint failed"
  exit 1
fi
