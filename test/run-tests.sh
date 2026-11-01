#!/usr/bin/env bash
# Run the org-linear ERT tests in batch mode.
# Usage: ./test/run-tests.sh [TEST-FILE...]
# With no arguments, runs every *-test.el file in test/.

set -euo pipefail

EMACS="${EMACS:-emacs}"
PROJECT_DIR="$(cd "$(dirname "$0")/.." && pwd)"

if [ $# -gt 0 ]; then
  TEST_FILES=("$@")
else
  TEST_FILES=("$PROJECT_DIR"/test/*-test.el)
fi

FAILED=0
for test_file in "${TEST_FILES[@]}"; do
  echo "=== Running $(basename "$test_file") ==="
  if ! "$EMACS" -Q --batch -L "$PROJECT_DIR" -L "$PROJECT_DIR/test" \
       -l ert -l "$test_file" -f ert-run-tests-batch-and-exit; then
    FAILED=$((FAILED + 1))
  fi
done

[ "$FAILED" -eq 0 ]
