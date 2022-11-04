from dataclasses import dataclass
import dataclasses
import json
import os
from typing import IO, Any, Dict
import quickstrom.protocol as protocol
from quickstrom.result import *
from quickstrom.reporter import Reporter
from pathlib import Path
from datetime import datetime


@dataclass(frozen=True)
class Report(Generic[I]):
    result: DiffedResult[I]
    generated_at: datetime


@dataclass
class JsonReporter(Reporter):
    path: Path
    files_dir: Path

    def report(self, result: PlainResult):
        result_with_paths = write_screenshots(result, self.path.parent, self.files_dir)
        report = Report(diff_result(result_with_paths), datetime.utcnow())
        encode_file(report, self.path)


def write_screenshots(result: PlainResult,
                      base: Path,
                      dir: Path) -> ResultWithScreenshots[Path]:
    os.makedirs(dir)
    def on_state(
        state: State[protocol.JsonLike,
                     bytes]) -> State[protocol.JsonLike, Path]:

        if state.screenshot:
            p = dir / Path(f"{state.hash}.png")
            p.write_bytes(state.screenshot.image)
            return State(state.hash, state.queries, Screenshot(p.relative_to(base), state.screenshot.width, state.screenshot.height, state.screenshot.scale))
        else:
            return State(state.hash, state.queries, None)

    return map_states(result, on_state)


def encode_str(report: Report) -> str:
    return json.dumps(report, cls=_ReporterEncoder)


def encode_to(report: Report, fp: IO[str]):
    json.dump(report, fp, cls=_ReporterEncoder)


def encode_file(report: Report, output_path: Path):
    with open(output_path, 'w') as f:
        encode_to(report, f)


class _ReporterEncoder(json.JSONEncoder):
    def default(self, o: Any):
        if isinstance(o, Report):
            return {
                'result': self.default(o.result),
                'generatedAt': str(o.generated_at),
                'tag': 'Report'
            }
        elif isinstance(o, Passed):
            return {
                'tag': 'Passed',
                'passedTests': [self.default(test) for test in o.passed_tests],
            }
        elif isinstance(o, Errored):
            return {
                'tag': 'Errored',
                'passedTests': [self.default(test) for test in o.passed_tests],
                'erroredTest': self.default(o.errored_test),
            }
        elif isinstance(o, Failed):
            return {
                'tag': 'Failed',
                'passedTests': [self.default(test) for test in o.passed_tests],
                'failedTest': self.default(o.failed_test)
            }
        elif isinstance(o, Test):
            return {
                'validity': self.default(o.validity),
                'transitions': [self.default(t) for t in o.transitions],
            }
        elif isinstance(o, Initial):
            return {
                'events': [self.default(t) for t in o.events],
                'state': o.state,
            }
        elif isinstance(o, StateTransition):
            return {
                'tag': 'StateTransition',
                'fromState': o.from_state,
                'toState': o.to_state,
                'actions': [self.default(t) for t in o.actions],
            }
        elif isinstance(o, ErrorTransition):
            return {
                'tag': 'ErrorTransition',
                'fromState': o.from_state,
                'actions': [self.default(t) for t in o.actions],
                'error': o.error,
            }
        elif isinstance(o, State):
            return {
                'hash':
                o.hash,
                'queries':
                o.queries,
                'screenshot':
                self.default(o.screenshot)
                if o.screenshot is not None else None
            }
        elif isinstance(o, Screenshot):
            return {
                'url': o.image,
                'width': o.width,
                'height': o.height,
                'scale': o.scale,
            }
        elif isinstance(o, protocol.Action):
            return {
                'id': o.id,
                'args': o.args,
                'isEvent': o.isEvent,
                'timeout': o.timeout
            }
        elif isinstance(o, protocol.Validity):
            return dataclasses.asdict(o)
        elif isinstance(o, Added):
            assert (isinstance(o.value, dict))
            o.value['diff'] = 'Added'
            return o.value
        elif isinstance(o, Removed):
            assert (isinstance(o.value, dict))
            o.value['diff'] = 'Removed'
            return o.value
        elif isinstance(o, Modified):
            assert (isinstance(o.value, dict))
            o.value['diff'] = 'Modified'
            return o.value
        elif isinstance(o, Unmodified):
            assert (isinstance(o.value, dict))
            o.value['diff'] = 'Unmodified'
            return o.value
        elif isinstance(o, Path):
            return str(o)
        else:
            return json.JSONEncoder.default(self, o)
