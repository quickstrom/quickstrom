import logging
import tempfile
from quickstrom.reporter import Reporter
import click
from typing import Tuple, cast, Dict, List, Optional
from urllib.parse import urljoin, urlparse
from pathlib import Path

import quickstrom.executor as executor
import quickstrom.reporter.json as json_reporter
import quickstrom.reporter.html as html_reporter
import quickstrom.reporter.console as console_reporter
from quickstrom.result import Errored, Failed, Passed


def ordinal(n: int) -> str:
    if n == 11:
        return "11th"
    elif n == 12:
        return "12th"
    else:
        r = n % 10
        if r == 1:
            return f"{n}st"
        elif r == 2:
            return f"2nd"
        else:
            return f"{n}th"


def format_after_passed_tests_str(passed_tests: List, suffix: str) -> str:
    l = len(passed_tests)
    if l == 0:
        return f"The {ordinal(l + 1)} test {suffix}"
    else:
        return f"After {l} passing tests, the {ordinal(l + 1)} test {suffix}"


class NoWebdriverFilter(logging.Filter):
    def filter(self, record):
        return not record.name.startswith('selenium.webdriver.remote')


global_options: Dict[str, object] = {'includes': []}


@click.group()
@click.option('--color',
              default='auto',
              help='use colored output (no|auto|always)')
@click.option('--log-level',
              default='WARN',
              help='log level (debug|info|warn|error)')
@click.option('-I',
              '--include',
              multiple=True,
              help='include a path in the Specstrom module search paths')
@click.pass_context
def root(ctx, color, log_level, include):
    if color.lower() == 'auto':
        ctx.color = None
    elif color.lower() == 'always':
        ctx.color = True
    elif color.lower() == 'no':
        ctx.color = False
    else:
        raise click.UsageError(f"Invalid color option: `{color}`")
    global_options['includes'] = include
    logging.basicConfig(
        format=
        '%(asctime)s.%(msecs)03d %(name)-24s %(levelname)-8s %(message)s',
        level=getattr(logging, log_level.upper()),
        datefmt='%Y-%m-%d %H:%M:%S')
    logging.getLogger("urllib3").setLevel(logging.INFO)
    logging.getLogger("selenium.webdriver.remote").setLevel(logging.INFO)


@click.command()
@click.argument('module')
@click.argument('origin')
@click.option('-B', '--browser', default='firefox')
@click.option('--browser-binary')
@click.option('--headless/--headful', default=True)
@click.option('-S',
              '--capture-screenshots/--no-capture-screenshots',
              default=False,
              help='capture a screenshot at each state and write to /tmp')
@click.option('--console-report-on-success/--no-console-report-on-success',
              default=False,
              help='capture a screenshot at each state and write to /tmp')
@click.option('--reporter',
              multiple=True,
              default=['console'],
              help='enable a reporter by name')
@click.option('--interpreter-log-file', default=None)
@click.option('--driver-log-file', default=None)
@click.option('--browser-log-file', default=None)
@click.option('--json-report-file', default='report.json')
@click.option('--json-report-files-directory',
              default='json-report-files',
              help='directory for report assets, e.g. screenshots')
@click.option('--html-report-directory', default='html-report')
@click.option(
    '--cookie',
    multiple=True,
    type=(str, str, str),
    help='set a cookie based on three values, e.g. --cookie domain name value')
@click.option('--browser-data-directory', default=None, help='directory for browser data files (user profile, etc)')
def check(module: str, origin: str, browser: executor.Browser, browser_binary: Optional[str], headless: bool,
          capture_screenshots: bool, console_report_on_success: bool,
          reporter: List[str], interpreter_log_file: Optional[str],
          driver_log_file: Optional[str],
          browser_log_file: Optional[str],
          json_report_file: str, json_report_files_directory: str,
          html_report_directory: str, cookie: List[Tuple[str, str, str]],
          browser_data_directory: Optional[str],
          ):
    """Checks the configured properties in the given module."""

    def reporters_by_names(names: List[str]) -> List[Reporter]:
        all_reporters = {
            'json':
                json_reporter.JsonReporter(Path(json_report_file),
                                           Path(json_report_files_directory)),
            'html':
                html_reporter.HtmlReporter(Path(html_report_directory)),
            'console':
                console_reporter.ConsoleReporter(console_report_on_success)
        }
        chosen_reporters = []
        for name in names:
            if name in all_reporters:
                chosen_reporters.append(all_reporters[name])
            else:
                raise click.UsageError(f"There is no reporter called `{name}`")
        return chosen_reporters

    origin_url = urlparse(urljoin("file://", origin))
    if origin_url.scheme == "file" and not Path(origin_url.path).is_file():
        print(f"File does not exist: {origin}")
        exit(1)

    def get_interpreter_log_file():
        if interpreter_log_file is None:
            return tempfile.NamedTemporaryFile(prefix="interpreter.", suffix=".log", delete=False)
        else:
            return open(str(interpreter_log_file), "w+")

    with get_interpreter_log_file() as ilog:
        try:
            print(f"Interpreter log: {ilog.name}")

            cookies = [
                executor.Cookie(domain, name, value)
                for (domain, name, value) in cookie
            ]
            results = executor.Check(module,
                                     origin_url.geturl(),
                                     browser,
                                     browser_binary,
                                     cast(List[str],
                                          global_options['includes']),
                                     headless,
                                     capture_screenshots,
                                     cookies,
                                     browser_data_directory=browser_data_directory,
                                     interpreter_log_file=ilog,
                                     driver_log_file=driver_log_file,
                                     browser_log_file=browser_log_file).execute()
            chosen_reporters = reporters_by_names(reporter)
            for result in results:
                for r in chosen_reporters:
                    r.report(result)

                click.echo("")

                if isinstance(result, Passed):
                    l = len(result.passed_tests)
                    if l == 1:
                        click.echo(click.style(f"The test passed.",
                                               fg="green"))
                    else:
                        click.echo(
                            click.style(f"All {l} tests passed.", fg="green"))
                if isinstance(result, Failed):
                    click.echo(
                        click.style(format_after_passed_tests_str(
                            result.passed_tests,
                            f"failed with {result.failed_test.validity.certainty} {result.failed_test.validity.value}."
                        ),
                            fg="red"))
                elif isinstance(result, Errored):
                    click.echo(
                        click.style(format_after_passed_tests_str(
                            result.passed_tests, f"errored!"),
                            fg="red"))

            if any([(isinstance(r, Errored)) for r in results]):
                exit(1)
            elif any([(isinstance(r, Failed)) for r in results]):
                exit(3)

        except executor.SpecstromAbortedError as err:
            print("\nCheck was aborted:\n")
            print(err)
            exit(2)
        except executor.SpecstromError as err:
            print(err)
            print(f"See interpreter log file for details: {ilog.name}")
            exit(2)


root.add_command(check)


def run():
    root()  # type: ignore
