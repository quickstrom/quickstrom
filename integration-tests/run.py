#!/usr/bin/env python3
import os
import subprocess
import sys
from typing import Optional
from dataclasses import dataclass

todomvc = os.getenv("TODOMVC_DIR")
dirname = os.path.dirname(__file__)

@dataclass
class Test():
    name: str
    module: str
    origin: str
    include: Optional[str]
    expected_exit_code: int

def run_test(browser: str, test: Test):
    assert browser in ["firefox", "chrome"]

    print(f"Running test '{test.name}'...")
    args = [arg for arg in [
        "quickstrom",
        "--log-level=INFO",
        "-I" + os.path.join(dirname, "../case-studies"),
        f"-I{test.include}",
        "check",
        test.module,
        test.origin,
        f"--browser={browser}",
        "--reporter=console",
        "--driver-log-file=driver.log",
        ]
        if arg != ""
    ]
    print("\n\t\033[3m" + " ".join(args) + "\033[0m\n")
    p = subprocess.run(args)
    if p.returncode is test.expected_exit_code:
        print(f"Integration test '{test.name}' exited as expected.")
    else:
        raise Exception(f"{test.name} failed, expected exit code {test.expected_exit_code} but got {p.returncode}")

def passing(name: str, origin: Optional[str] = None):
    origin = origin or f"{dirname}/passing/{name}.html"
    return Test(name=name, module=name, origin=origin, include=f"{dirname}/passing", expected_exit_code=0)

def failing(name: str, expected_exit_code: int, origin: Optional[str] = None):
    origin = f"{dirname}/failing/{name}.html"
    return Test(name=name, module=name, origin=origin, include=f"{dirname}/failing", expected_exit_code=expected_exit_code)

tests = [
    passing("todomvc", origin=f"{todomvc}/examples/backbone/index.html"),
    passing("liveness"),
    passing("async-change"),
    passing("async-css-change"),
    passing("only-noop"),
    passing("select"),
    failing("invalid-spec", expected_exit_code=2),
    passing("scroll"),
    failing("not-interactable", expected_exit_code=1),
]

def run_tests(browser: str):
    for test in tests:
        run_test(browser, test)

if __name__ == "__main__":
    browser = None
    if len(sys.argv) >= 2:
        browser = sys.argv[1]
    else:
        browser = "chrome"
        print(f"No browser specified, defaulting to '{browser}'.")

    run_tests(browser)
