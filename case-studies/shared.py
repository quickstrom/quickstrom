import sys
from urllib.parse import urljoin
import click
import os
from typing import Literal, Union, Optional, TextIO
from dataclasses import dataclass
import pathlib
import subprocess

case_studies_dir = pathlib.Path(__file__).parent.resolve()

ResultName = Union[Literal['passed'], Literal['failed'], Literal['error'],
                   Literal['specstrom-error'], Literal['unknown'], ]


@dataclass
class TestApp():
    name: str
    module: str
    origin: str
    expected: ResultName

    def origin_url(self):
        return urljoin("file://", self.origin)


def heading1(s):
    return click.style(s, bold=True, underline=True)


def heading2(s):
    return click.style(s, bold=True)


def success(s):
    return click.style(s, fg='green')


def failure(s):
    return click.style(s, fg='red')


def warning(s):
    return click.style(s, fg='yellow')


case_studies_dir = pathlib.Path(__file__).parent.resolve()


def result_from_exit_code(n: int) -> ResultName:
    if n == 0:
        return 'passed'
    elif n == 1:
        return 'error'
    elif n == 2:
        return 'specstrom-error'
    elif n == 3:
        return 'failed'
    else:
        return 'unknown'


def todomvc_server():
    todomvc_dir = os.getenv("TODOMVC_DIR")
    if todomvc_dir is None:
        raise Exception("Missing TODOMVC_DIR environment variable")
    return subprocess.Popen(
        ["python3", "-m", "http.server", "--directory", todomvc_dir, "12345"],
        stdout=subprocess.DEVNULL,
        stderr=subprocess.DEVNULL)


include_paths = [str(p.absolute()) for p in [case_studies_dir]]

Browser = Union[Literal["firefox"], Literal["chrome"]]


def check(app: TestApp,
          browser: Browser = "firefox",
          html_report_dir: Optional[str] = None,
          interpreter_log_file: Optional[str] = None,
          driver_log_file: Optional[str] = None,
          headful: bool = False,
          stdout: TextIO = sys.stdout,
          stderr: TextIO = sys.stderr):

    include_flags = [arg for path in include_paths for arg in ["-I", path]]

    def optional(name, value):
        if value is not None:
            return [name, value]
        else:
            return []

    args = ["quickstrom"] + include_flags + [
        "--log-level=debug",
        "check",
        app.module,
        app.origin_url(),
        f"--browser={browser}",
        "--reporter=console",
        "--capture-screenshots",
        "--headful" if headful else "--headless",
    ] + optional("--reporter", "html" if html_report_dir is not None else None) \
        + optional("--html-report-directory", html_report_dir) \
            + optional("--interpreter-log-file", interpreter_log_file) \
            + optional("--driver-log-file", driver_log_file)

    click.echo(f"Command: {' '.join(args)}")
    click.echo("")

    p = subprocess.Popen(args, stdout=stdout, stderr=stderr)

    return result_from_exit_code(p.wait())
