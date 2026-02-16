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

echo "Test: run_bg_and_wait suppresses ERR trap only for wait path"
err_hits=0
trap 'err_hits=$((err_hits + 1))' ERR
( exit 7 ) &
pid_trap=$!
run_bg_and_wait "$pid_trap" status_trap
assert_eq 7 "$status_trap" "status for background command with exit 7"
assert_eq 0 "$err_hits" "ERR trap should not fire during wait"

# verify trap handling is restored after run_bg_and_wait returns
set +e
false
set -eE
assert_eq 1 "$err_hits" "ERR trap should be restored after wait"
trap - ERR

echo "Test: trap handlers attempt SQLite FAILED update"
tmp_root="$(mktemp -d)"
mkdir -p "${tmp_root}/R_HOME/bin"
status_trace="${tmp_root}/status_trace.log"
touch "${tmp_root}/upd_job_status.R" "${tmp_root}/tracking.sqlite"

cat > "${tmp_root}/R_HOME/bin/Rscript" <<'EOF'
#!/usr/bin/env bash
status=""
job_id=""
for ((i=1; i<=$#; i++)); do
  if [[ "${!i}" == "--status" ]]; then
    next=$((i+1))
    status="${!next}"
  elif [[ "${!i}" == "--job_id" ]]; then
    next=$((i+1))
    job_id="${!next}"
  fi
done

if [[ -n "${BG_TEST_STATUS_TRACE:-}" ]]; then
  echo "${job_id}:${status}" >> "${BG_TEST_STATUS_TRACE}"
fi
exit 0
EOF
chmod +x "${tmp_root}/R_HOME/bin/Rscript"

R_HOME="${tmp_root}/R_HOME"
upd_job_status_path="${tmp_root}/upd_job_status.R"
sqlite_db="${tmp_root}/tracking.sqlite"
BG_TEST_STATUS_TRACE="${status_trace}"
SLURM_JOB_ID="9876"
complete_file="${tmp_root}/.traptest_complete"
export R_HOME upd_job_status_path sqlite_db BG_TEST_STATUS_TRACE SLURM_JOB_ID complete_file

set +e
( traperror trap_step 9 1 1 "false" "::" >/dev/null 2>&1 )
trap_err_rc=$?
set -eE
assert_eq 9 "$trap_err_rc" "traperror exits with supplied error code"
assert_eq "9876:FAILED" "$(tail -n 1 "${status_trace}")" "traperror should update FAILED in SQLite"

set +e
( trap_job_failure trap_step SIGTERM >/dev/null 2>&1 )
trap_sig_rc=$?
set -eE
assert_eq 1 "$trap_sig_rc" "trap_job_failure exits with status 1"
assert_eq "9876:FAILED" "$(tail -n 1 "${status_trace}")" "trap_job_failure should update FAILED in SQLite"

unset R_HOME upd_job_status_path sqlite_db BG_TEST_STATUS_TRACE SLURM_JOB_ID complete_file
rm -rf "${tmp_root}"

if [[ "$fail_count" -ne 0 ]]; then
  echo "FAILED: $fail_count assertions"
  exit 1
fi

echo "All run_bg_and_wait tests passed."
