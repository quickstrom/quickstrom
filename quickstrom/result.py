import quickstrom.protocol as protocol
from quickstrom.hash import dict_hash
from dataclasses import dataclass
from typing import Callable, Dict, Generic, List, Optional, Tuple, Type, TypeVar, Union

Selector = str

I = TypeVar('I')
O = TypeVar('O')


@dataclass(frozen=True, eq=True)
class Screenshot(Generic[I]):
    image: I
    width: int
    height: int
    scale: int


T = TypeVar('T')


@dataclass(frozen=True, eq=True)
class Added(Generic[T]):
    value: T


@dataclass(frozen=True, eq=True)
class Removed(Generic[T]):
    value: T


@dataclass(frozen=True, eq=True)
class Modified(Generic[T]):
    value: T


@dataclass(frozen=True, eq=True)
class Unmodified(Generic[T]):
    value: T


Diff = Union[Added[T], Removed[T], Modified[T], Unmodified[T]]

T2 = TypeVar('T2')


def map_diff(f: Callable[[T], T2], diff: Diff[T]) -> Diff[T2]:
    if isinstance(diff, Added):
        return Added(f(diff.value))
    elif isinstance(diff, Removed):
        return Removed(f(diff.value))
    elif isinstance(diff, Modified):
        return Modified(f(diff.value))
    elif isinstance(diff, Unmodified):
        return Unmodified(f(diff.value))
    else:
        raise TypeError(f"{diff} is not a Diff")


def new_value(diff: Diff[T2]) -> T2:
    if isinstance(diff, Added):
        return diff.value
    elif isinstance(diff, Removed):
        return diff.value
    elif isinstance(diff, Modified):
        return diff.value
    elif isinstance(diff, Unmodified):
        return diff.value


E = TypeVar('E')
E2 = TypeVar('E2')


@dataclass(frozen=False)
class State(Generic[E, I]):
    hash: str
    queries: Dict[Selector, List[E]]
    screenshot: Optional[Screenshot[I]]


@dataclass(frozen=True)
class Initial(Generic[E, I]):
    events: List[protocol.Action]
    state: State[E, I]


@dataclass(frozen=True)
class StateTransition(Generic[E, I]):
    from_state: Optional[State[E, I]]
    to_state: State[E, I]
    actions: List[protocol.Action]


@dataclass(frozen=True)
class ErrorTransition(Generic[E, I]):
    from_state: Optional[State[E, I]]
    actions: List[protocol.Action]
    error: str


Transition = Union[StateTransition[E, I], ErrorTransition[E, I]]


@dataclass(frozen=True)
class Test(Generic[E, I]):
    validity: protocol.Validity
    transitions: List[Transition[E, I]]


@dataclass(frozen=True)
class Errored(Generic[E, I]):
    passed_tests: List[Test[E, I]]
    errored_test: Test[E, I]


@dataclass(frozen=True)
class Failed(Generic[E, I]):
    passed_tests: List[Test[E, I]]
    failed_test: Test[E, I]


@dataclass(frozen=True)
class Passed(Generic[E, I]):
    passed_tests: List[Test[E, I]]


Result = Union[Failed[E, I], Passed[E, I], Errored]

ResultWithScreenshots = Result[protocol.JsonLike, I]

PlainResult = ResultWithScreenshots[bytes]


def map_states(r: Result[E, I], f: Callable[[State[E, I]],
                                            State[E2, O]]) -> Result[E2, O]:
    def on_transition(t: Transition[E, I]) -> Transition[E2, O]:
        if isinstance(t, StateTransition):
            return StateTransition(
                f(t.from_state) if t.from_state else None, f(t.to_state),
                t.actions)
        elif isinstance(t, ErrorTransition):
            return ErrorTransition(
                f(t.from_state) if t.from_state else None, t.actions, t.error)

    def on_test(test: Test[E, I]) -> Test[E2, O]:
        return Test(test.validity, [on_transition(t) for t in test.transitions]) # type: ignore

    if isinstance(r, Passed):
        return Passed([on_test(test) for test in r.passed_tests])
    elif isinstance(r, Failed):
        return Failed([on_test(test) for test in r.passed_tests],
                      on_test(r.failed_test))
    elif isinstance(r, Errored):
        return Errored(list(map(on_test, r.passed_tests)),
                       on_test(r.errored_test))


def from_state(state: protocol.State) -> State[protocol.JsonLike, bytes]:
    return State(dict_hash(state), state, None)


def transitions_from_trace(
        full_trace: protocol.Trace
) -> List[Transition[protocol.JsonLike, bytes]]:
    A = TypeVar('A')
    B = TypeVar('B')
    trace = list(full_trace.copy())

    def pop_either(a: Type[A], b: Type[B]) -> Union[A, B]:
        assert len(trace) > 0
        first = trace.pop(0)
        if isinstance(first, a) or isinstance(first, b):
            return first
        else:
            raise TypeError(
                f"Expected {a} or {b} in trace but got {type(first)}")

    transitions: List[Transition[protocol.JsonLike, bytes]] = []
    last_state: Optional[State] = None
    while len(trace) > 0:
        actions = pop_either(protocol.TraceActions, protocol.TraceError)
        if isinstance(actions, protocol.TraceError):
            transitions.append(
                ErrorTransition(from_state=last_state,
                                actions=[],
                                error=actions.error))
            break

        last = pop_either(protocol.TraceState, protocol.TraceError)
        if isinstance(last, protocol.TraceError):
            transitions.append(
                ErrorTransition(from_state=last_state,
                                actions=actions.actions,
                                error=last.error))
            break
        elif isinstance(last, protocol.TraceState):
            to_state = from_state(last.state)
            transitions.append(
                StateTransition(
                    from_state=last_state,
                    to_state=to_state,
                    actions=actions.actions,
                ))
            last_state = to_state

    return transitions


def from_protocol_result(result: protocol.Result) -> PlainResult:
    if isinstance(result, protocol.RunResult):
        if result.valid.value:
            return Passed(
                [Test(result.valid, transitions_from_trace(result.trace))]) # type: ignore
        else:
            return Failed([],
                          Test(result.valid,
                               transitions_from_trace(result.trace))) # type: ignore
    elif isinstance(result, protocol.ErrorResult):
        return Errored([],
                       Test(protocol.Validity('Definitely', False),
                            transitions_from_trace(result.trace))) # type: ignore


DiffedResult = Union[Failed[Diff[protocol.JsonLike],
                            I], Passed[Diff[protocol.JsonLike], I],
                     Errored[Diff[protocol.JsonLike], I]]


def diff_states(
    old: State[protocol.JsonLike, I], new: State[protocol.JsonLike, I]
) -> Tuple[State[Diff[protocol.JsonLike], I], State[Diff[protocol.JsonLike],
                                                    I]]:
    old_result_queries = {}
    new_result_queries = {}

    for sel in new.queries.keys():
        old_elements: List[Dict[str, protocol.JsonLike]] = old.queries.get(
            sel, [])    # type: ignore
        new_elements: List[Dict[str, protocol.JsonLike]] = new.queries.get(
            sel, [])    # type: ignore

        old_by_ref = {el['ref']: el for el in old_elements}
        new_by_ref = {el['ref']: el for el in new_elements}

        def diff_old(
                el: Dict[str, protocol.JsonLike]) -> Diff[protocol.JsonLike]:
            ref = el['ref']
            if ref in new_by_ref:
                new_el = new_by_ref[ref]
                if new_el == el:
                    return Unmodified(el)
                else:
                    return Modified(el)
            else:
                return Removed(el)

        def diff_new(
                el: Dict[str, protocol.JsonLike]) -> Diff[protocol.JsonLike]:
            ref = el['ref']
            if ref in old_by_ref:
                old_el = old_by_ref[ref]
                if old_el == el:
                    return Unmodified(el)
                else:
                    return Modified(el)
            else:
                return Added(el)

        old_result_queries[sel] = [diff_old(el) for el in old_elements]
        new_result_queries[sel] = [diff_new(el) for el in new_elements]

    return (State(old.hash, old_result_queries, old.screenshot),
            State(new.hash, new_result_queries, new.screenshot))


def diff_transitions(
    ts: List[Transition[protocol.JsonLike, I]]
) -> List[Transition[Diff[protocol.JsonLike], I]]:
    results: List[Transition[Diff[protocol.JsonLike], I]] = []
    last_state = None
    for t in ts:
        if isinstance(t, StateTransition):

            def _diff_states():
                assert isinstance(t, StateTransition)
                if last_state is None:
                    return (None, mark_unmodified(t.to_state))
                else:
                    return diff_states(last_state, t.to_state)

            (diff_old, diff_new) = _diff_states()
            results.append(
                StateTransition(from_state=diff_old,
                                to_state=diff_new,
                                actions=t.actions))
            last_state = t.to_state
        elif isinstance(t, ErrorTransition):
            results.append(
                ErrorTransition(
                    mark_unmodified(t.from_state) if t.from_state else None,
                    t.actions, t.error))

    return results


def diff_test(
        test: Test[protocol.JsonLike, I]) -> Test[Diff[protocol.JsonLike], I]:
    return Test(test.validity, diff_transitions(test.transitions)) # type: ignore


def diff_result(result: ResultWithScreenshots[I]) -> DiffedResult[I]:
    if isinstance(result, Errored):
        return result
    elif isinstance(result, Failed):
        return Failed([diff_test(test) for test in result.passed_tests],
                      diff_test(result.failed_test))
    elif isinstance(result, Passed):
        return Passed([diff_test(test) for test in result.passed_tests])


def mark_unmodified(s: State[E, I]) -> State[Diff[E], I]:
    return State(
        s.hash,
        {sel: [Unmodified(e) for e in es]
         for sel, es in s.queries.items()}, s.screenshot)


def mark_all_unmodified(r: Result[E, I]) -> Result[Diff[E], I]:
    return map_states(r, mark_unmodified)
