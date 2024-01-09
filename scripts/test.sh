#!/usr/bin/env bash
set -euo pipefail
cd "$(git rev-parse --show-toplevel)"

run_tests() {
  # eg. scripts/test.sh --match "D01" --match "D02"
  arr=()
  for i in "$@"; do
    arr+=("--test-option" "$i")
  done

  cabal --ghc-options -Wwarn test --test-option="--times" "${arr[@]}"
}

cabal build --enable-tests
time run_tests "$@"
