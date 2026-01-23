#!/bin/bash
set -eE

script_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
repo_root="$(cd "${script_dir}/.." && pwd)"

# shellcheck source=/dev/null
source "${repo_root}/inst/shell_functions"

fail_count=0

assert_eq() {
  local expected="$1"
  local actual="$2"
  local msg="$3"

  if [[ "$expected" != "$actual" ]]; then
    echo "ASSERT FAIL: $msg (expected=$expected actual=$actual)"
    fail_count=$((fail_count + 1))
  else
    echo "ok: $msg"
  fi
}

echo "Test: run_bg_and_wait returns 0 for success"
sleep 0.1 &
pid_ok=$!
run_bg_and_wait "$pid_ok" status_ok
assert_eq 0 "$status_ok" "status for successful background job"

echo "Test: run_bg_and_wait returns non-zero for failure"
( exit 2 ) &
pid_fail=$!
run_bg_and_wait "$pid_fail" status_fail
assert_eq 2 "$status_fail" "status for failing background job"

echo "Test: run_bg_and_wait does not trigger set -eE"
set -eE
false &
pid_err=$!
run_bg_and_wait "$pid_err" status_err
assert_eq 1 "$status_err" "status for background 'false' command"

if [[ "$fail_count" -ne 0 ]]; then
  echo "FAILED: $fail_count assertions"
  exit 1
fi

echo "All run_bg_and_wait tests passed."
