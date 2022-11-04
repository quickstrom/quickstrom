#!/usr/bin/env python3

import sys
from typing import List
import shutil
import click
import time
import os
import pathlib

import shared


def run(results_root: str, apps: List[shared.TestApp]):
    with shared.todomvc_server() as server:
        unexpected_result_tests = []
        try:
            shutil.rmtree(results_root, ignore_errors=True)
            os.makedirs(results_root)
            browsers: List[shared.Browser] = [
                # "chrome",
                "firefox"
            ]
            for app in apps:

                for browser in browsers:
                    max_tries = 5
                    for try_n in range(1, max_tries + 1):
                        result_dir = str(
                            pathlib.Path(
                                f"{results_root}/{app.name}.{browser}.{try_n}"
                            ).absolute())
                        os.makedirs(result_dir)
                        html_report_dir = f"{result_dir}/html-report"
                        interpreter_log_file = f"{result_dir}/interpreter.log"
                        driver_log_file = f"{result_dir}/driver.log"
                        duration_file = f"{result_dir}/duration"
                        start_time = time.time()
                        shutil.rmtree(html_report_dir, ignore_errors=True)

                        with open(f"{result_dir}/stdout.log",
                                  "w") as stdout_file:
                            with open(f"{result_dir}/stderr.log",
                                      "w") as stderr_file:
                                click.echo(shared.heading1(f"{app.name}"))
                                click.echo(f"Browser: {browser}")
                                click.echo("Stdout: " + stdout_file.name)
                                click.echo("Stderr: " + stderr_file.name)
                                click.echo(
                                    f"Interpreter log: {interpreter_log_file}")
                                click.echo(
                                    f"Driver log: {driver_log_file}")
                                click.echo(
                                    f"HTML report: {html_report_dir}/index.html"
                                )

                                click.echo(f"Try {try_n}...")
                                try:
                                    r = shared.check(
                                        app=app,
                                        html_report_dir=html_report_dir,
                                        interpreter_log_file=
                                        interpreter_log_file,
                                        driver_log_file=driver_log_file,
                                        stdout=stdout_file,
                                        stderr=stderr_file,
                                        browser=browser,
                                        headful=False)

                                    if r != app.expected:
                                        if try_n == max_tries or app.expected == 'passed':
                                            unexpected_result_tests.append(
                                                app.name)
                                        click.echo(
                                            shared.failure(
                                                f"Expected '{app.expected}' but result was '{r}'!"
                                            ))
                                        if app.expected == 'passed':
                                            break
                                    else:
                                        if r == 'passed':
                                            click.echo(
                                                shared.success(f"Got expected '{r}'!"))
                                        else:
                                            click.echo(
                                                shared.warning(
                                                    f"Got expected '{r}'!"))
                                        break
                                except KeyboardInterrupt:
                                    exit(1)
                                except Exception as e:
                                    click.echo(
                                        f"Test failed with exception:\n{e}",
                                        file=stdout_file)
                                    click.echo(
                                        shared.failure(
                                            "result: failed with exception"))
                                finally:
                                    end_time = time.time()
                                    with open(duration_file, 'w+') as f:
                                        f.write(str(end_time - start_time))

                                click.echo("")
        finally:
            server.kill()
            if len(unexpected_result_tests) > 0:
                click.echo(
                    f"There were unexpected results. Rerun only those apps with:\n\n{sys.argv[0]} {sys.argv[1]} {' '.join(unexpected_result_tests)}"
                )
                exit(1)
            else:
                click.echo("All results were as expected!")


def todomvc_app(name: str,
                path: str = "index.html",
                expected: shared.ResultName = 'passed') -> shared.TestApp:
    base = "http://localhost:12345"
    url = f"{base}/examples/{name}/{path}"
    return shared.TestApp(name, os.getenv("TODOMVC_SPEC_MODULE", "todomvc"), url, expected)


all_apps = [
    todomvc_app("angular-dart", path="web/", expected='failed'),
    todomvc_app("angular2_es2015", expected='failed'),
    todomvc_app("angular2", expected='failed'),
    todomvc_app("angularjs_require"),
    todomvc_app("angularjs", expected='failed'),
    todomvc_app("aurelia", expected='failed'),
    todomvc_app("backbone_marionette", expected='failed'),
    todomvc_app("backbone_require"),
    todomvc_app("backbone"),
    todomvc_app("binding-scala"),
    todomvc_app("canjs_require", expected='failed'),
    todomvc_app("canjs", expected='failed'),
    todomvc_app("closure"),
    todomvc_app("dijon", expected='failed'),
    todomvc_app("dojo", expected='failed'),
    todomvc_app("duel", path="www/index.html", expected='failed'),
    todomvc_app("elm", expected='failed'),
    todomvc_app("emberjs", expected='failed'),
    todomvc_app("enyo_backbone"),
    todomvc_app("exoskeleton"),
    todomvc_app("jquery", expected='failed'),
    todomvc_app("js_of_ocaml"),
    todomvc_app("jsblocks"),
    todomvc_app("knockback"),
    todomvc_app("knockoutjs_require", expected='failed'),
    todomvc_app("knockoutjs"),
    todomvc_app("kotlin-react"),
    todomvc_app("lavaca_require", expected='failed'),
    todomvc_app("mithril", expected='failed'),
    todomvc_app("polymer", expected='failed'),
    todomvc_app("ractive", expected='failed'),
    todomvc_app("react-alt"),
    todomvc_app("react-backbone"),
    todomvc_app("react"),
    todomvc_app("reagent", expected='failed'),
    todomvc_app("riotjs"),
    todomvc_app("scalajs-react"),
    todomvc_app("typescript-angular"),
    todomvc_app("typescript-backbone"),
    todomvc_app("typescript-react"),
    todomvc_app("vanilla-es6", expected='failed'),
    todomvc_app("vanillajs", expected='failed'),
    todomvc_app("vue"),
]

if __name__ == "__main__":
    if len(sys.argv) < 2:
        click.echo(f"Usage: {sys.argv[0]} RESULTS_DIR [APP]")
    else:
        results_root = sys.argv[1] or "results"
        apps_to_run = sys.argv[2:]
        selected_apps: List[shared.TestApp] = all_apps
        if len(apps_to_run) > 0:
            click.echo("Running selected apps only:")
            selected_apps = list(
                filter(lambda a: a.name in apps_to_run, all_apps))
        else:
            click.echo("Running all apps:")

        for app in selected_apps:
            click.echo(f" - {app.name}")

        click.echo("")

        run(results_root, selected_apps)
