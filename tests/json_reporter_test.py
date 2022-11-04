from datetime import datetime
from typing import List
import json
import quickstrom.result as result
import quickstrom.reporter.json as json_reporter
import quickstrom.protocol as protocol
from .strategies import *
from hypothesis import given


def protocol_result_from_result(r: result.PlainResult) -> protocol.Result:
    def to_trace(test: result.Test[protocol.JsonLike, bytes]) -> protocol.Trace:
        def to_state(state: result.State[protocol.JsonLike, bytes]) -> protocol.State:
            return state.queries

        def to_trace_elements(
                transition: result.Transition[protocol.JsonLike, bytes]) -> List[protocol.TraceElement]:
            return [
                protocol.TraceActions(transition.actions),
                protocol.TraceState(to_state(transition.to_state))
                    if isinstance(transition, result.StateTransition)
                    else protocol.TraceError(transition.error),
            ]

        return [e for t in test.transitions for e in to_trace_elements(t)]

    if isinstance(r, result.Passed):
        # TODO: we need to support multiple tests later on
        test = r.passed_tests[0]
        return protocol.RunResult(test.validity, to_trace(test))
    elif isinstance(r, result.Failed):
        # TODO: we need to support multiple tests later on
        test = r.failed_test
        return protocol.RunResult(test.validity, to_trace(test))
    elif isinstance(r, result.Errored):
        return protocol.ErrorResult(to_trace(r.errored_test))
    else:
        raise TypeError(f"Invalid result: {r}")


@given(results())
def test_result_roundtrip(protocol_result):
    r = result.from_protocol_result(protocol_result)
    new_protocol_result = protocol_result_from_result(r)
    assert new_protocol_result == protocol_result


@given(results())
def test_json_report_is_serializable(protocol_result: protocol.Result):
    report = json_reporter.Report(
        result.diff_result(result.from_protocol_result(protocol_result)),
        datetime.utcnow())
    json.loads(json_reporter.encode_str(report))
