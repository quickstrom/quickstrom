#!/usr/bin/env bash

WEBCHECK=${WEBCHECK:-cabal run quickstrom --}

geckodriver > /dev/null 2>&1 &
geckodriver_pid="$!"

# shellcheck disable=SC2064
trap "kill $geckodriver_pid" EXIT

echo "Geckodriver running ($geckodriver_pid)..."

function run_test {
    spec_file="$1"
    uri="$2"
    options="${3:---tests 10 --max-actions 100}"
    expected_exit_code="$4"
    echo "################################################################################"
    echo "# Specification: $spec_file"
    echo "# Origin:        $uri"
    echo "# Options:       $options"
    echo -e "################################################################################\n"

    # shellcheck disable=SC2086
    $WEBCHECK "$spec_file" "$uri" $options
    exit_code=$?

    if [ $exit_code == "$expected_exit_code" ]; then
        echo "Expected exit code ($expected_exit_code) was returned."
    else 
        echo "Expected exit code $expected_exit_code, but $exit_code was returned."
        exit 1
    fi
}

function run_group {
    dir="$1"
    options="$2"
    expected_exit_code="$3"

    for page in "$dir"/*.html; do
        run_test "${page%.*}.spec.purs" "$page" "$options" "$expected_exit_code"
    done
}

echo -e "Running Quickstrom integration tests...\n"
run_group "specs/passing" "--tests=5 --max-actions=50" 0
run_group "specs/failing" "--tests=50 --max-actions=100 --shrink-levels=0" 3
run_test "specs/other/TodoMVC.spec.purs" http://todomvc.com/examples/vue/ "--max-trailing-state-changes=0" 0
run_test "specs/other/TodoMVC.spec.purs" http://todomvc.com/examples/angularjs/ "--shrink-levels=0" 3