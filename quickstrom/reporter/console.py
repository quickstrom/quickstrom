from dataclasses import dataclass
from quickstrom.protocol import JsonLike
from quickstrom.reporter import Reporter
import sys
from typing import Any, Callable, IO, Text, Tuple
from quickstrom.result import *
import quickstrom.printer as printer
from deepdiff import DeepDiff
from tabulate import tabulate
import click


def element_heading(s: str) -> str:
    return click.style(s, bold=True, underline=True)


def selector(s: str) -> str:
    return click.style(f"`{s}`")


def added(s: str) -> str:
    return click.style(s, fg='green')


def removed(s: str) -> str:
    return click.style(s, fg='red')


def modified(s: str) -> str:
    return click.style(s, fg='blue')


def unmodified(s: str):
    return click.style(s, dim=True)


def indent(s: str, level: int) -> str:
    return f"{' ' * level * 2}{s}"


def errored(s: str) -> str:
    return click.style(s, fg='red')


def print_state_diff(transition: StateTransition[Diff[JsonLike], bytes],
                     file: Optional[IO[Text]]):
    def without_internal_props(
            d: Dict[Selector, JsonLike]) -> Dict[Selector, JsonLike]:
        return {
            key: value
            for key, value in d.items() if key not in ['ref', 'position']
        }

    def format_value(value: JsonLike) -> str:
        if isinstance(value, dict):
            kvs = [
                f"{key}: {format_value(item)}"
                for key, item in without_internal_props(value).items()
            ]
            return "{" + ", ".join(kvs) + "}"

        elif isinstance(value, list):
            vs = [f"{format_value(item)}" for item in value]
            return "[" + ", ".join(vs) + "]"
        elif isinstance(value, str):
            return '"' + value.replace('"', '\\"') + '"'
        elif isinstance(value, bool):
            return "true" if value else "false"
        elif value is None:
            return "null"
        else:
            return repr(value)

    def element_color(element_diff: Diff[JsonLike]) -> Callable[[str], str]:
        if isinstance(element_diff, Added):
            return added
        elif isinstance(element_diff, Removed):
            return removed
        elif isinstance(element_diff, Modified):
            return modified
        else:
            return unmodified

    def format_state(element_diff: Diff[JsonLike]) -> str:
        element = element_diff.value
        assert isinstance(element, dict)
        color = element_color(element_diff)
        attrs = [
            color(f"{key}: {format_value(value)}")
            for key, value in without_internal_props(element).items()
        ]
        return color(click.style(f"{element['ref']}",
                                 bold=True)) + "\n" + "\n".join(attrs)

    def format_states(elements: List[Diff[JsonLike]]) -> List[str]:
        return [format_state(element) for element in elements]

    from_queries = {} if transition.from_state is None else transition.from_state.queries
    for sel, to_states in transition.to_state.queries.items():
        from_states = from_queries.get(sel, [])
        click.echo("\n" + selector(sel), file=file)
        rows = [[
            a, b
        ] for a, b in zip(format_states(from_states), format_states(to_states))
                ]
        if len(rows) > 0:
            click.echo(tabulate(tabular_data=rows, tablefmt='fancy_grid'),
                       file=file)
        else:
            click.echo(click.style("No elements matched.", dim=True),
                       file=file)


@dataclass
class ConsoleReporter(Reporter):
    report_on_success: bool
    file: Optional[IO[Text]] = sys.stdout

    def report_test(self, test: Test[Diff[protocol.JsonLike], bytes]):
        click.echo("Trace:", file=self.file)
        for i, transition in enumerate(test.transitions):
            click.echo(element_heading(f"\nTransition #{i}"), file=self.file)
            click.echo(f"\nActions and events:", file=self.file)
            for action in transition.actions:
                click.echo(f"\n- {printer.pretty_print_action(action)}",
                           file=self.file)

            if isinstance(transition, StateTransition):
                click.echo(f"\nState difference:", file=self.file)
                print_state_diff(transition, file=self.file)
            elif isinstance(transition, ErrorTransition):
                click.echo("\nError:\n", file=self.file)
                click.echo(errored(transition.error), file=self.file)

    def report(self, result: PlainResult):
        if isinstance(result, Failed):
            diffed_test = diff_test(result.failed_test)
            self.report_test(diffed_test)
        elif isinstance(result, Errored):
            diffed_test = diff_test(result.errored_test)
            self.report_test(diffed_test)
        elif isinstance(result, Passed) and self.report_on_success:
            for test in result.passed_tests:
                diffed_test = diff_test(test)
                self.report_test(diffed_test)
