#!/usr/bin/env bash

geckodriver > /dev/null 2>&1 &
geckodriver_pid="$!"
trap "kill $geckodriver_pid" EXIT

echo "Geckodriver running ($geckodriver_pid)..."

function run_group {
    dir="$1"
    expected_exit_code="$2"

    for page in "$dir"/*.html; do
        spec_file="${page%.*}.spec.purs"
        echo -e "Specification: $spec_file\nOrigin: $page\n"
        cabal run webcheck -- "$spec_file" "$page" --tests 5 --max-actions 50
        exit_code=$?
        if [ $exit_code == "$expected_exit_code" ]; then
            echo "Expected exit code ($expected_exit_code) was returned."
        else 
            echo "Expected exit code $expected_exit_code, but $exit_code was returned."
            exit 1
        fi
    done
}
echo "Running WebCheck integration tests..."
run_group "specs/passing" 0
run_group "specs/failing" 1