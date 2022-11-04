#!/usr/bin/env python3

from dataclasses import dataclass
import sys
from typing import List, TextIO
import click
import time

import shared


@dataclass
class Result():
    name: str
    actual: shared.ResultName
    expected: shared.ResultName
    duration: float
    browser: str
    subscript: int

    def to_csv_row(self) -> str:
        # name,actual,expected,duration,browser,subscript
        return f"{self.name},{self.actual},{self.expected},{self.duration},{self.browser},{self.subscript}\n"


def measure(apps: List[shared.TestApp], subscript: int, file: TextIO):
    with shared.todomvc_server() as server:
        try:
            browsers: List[shared.Browser] = [
            # "chrome",
                "firefox",
            ]
            for app in apps:

                for browser in browsers:
                    runs = 10
                    for _ in range(1, runs + 1):
                        start_time = time.time()
                        click.echo(shared.heading1(f"{app.name}"))
                        click.echo(f"Browser: {browser}")

                        try:
                            r = shared.check(
                                app=app,
                                stderr=sys.stdout,
                                headful=False,
                                browser=browser,
                            )
                            end_time = time.time()
                            result = Result(app.name,
                                            actual=r,
                                            expected=app.expected,
                                            duration=(end_time - start_time),
                                            browser=browser,
                                            subscript=subscript)
                            file.write(result.to_csv_row())
                            file.flush()
                            click.echo(result)
                        except KeyboardInterrupt:
                            exit(1)
                        except Exception as e:
                            click.echo(f"Test failed with exception:\n{e}")
        finally:
            server.kill()


def todomvc_app(name: str,
                path: str = "index.html",
                expected: shared.ResultName = 'passed') -> shared.TestApp:
    base = "http://localhost:12345"
    url = f"{base}/examples/{name}/{path}"
    return shared.TestApp(name, "todomvc", url, expected)


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
    if len(sys.argv) < 3:
        click.echo(f"Usage: {sys.argv[0]} SUBSCRIPT OUTPUT_FILE [APPS]")
        exit(1)
    subscript = int(sys.argv[1])
    output_file = sys.argv[2]
    apps_to_run = sys.argv[3:]
    selected_apps: List[shared.TestApp] = all_apps
    if len(apps_to_run) > 0:
        selected_apps = list(filter(lambda a: a.name in apps_to_run, all_apps))
        click.echo("Running selected apps only:")
        for a in selected_apps:
            click.echo(f" - {a.name}")
    else:
        click.echo("Running all apps")
    click.echo("")

    with open(output_file, "w+") as f:
        measure(selected_apps, subscript=subscript, file=f)
