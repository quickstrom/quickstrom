import { h, FunctionComponent } from 'preact';
import Preact from 'preact';
import { useReducer, useState, StateUpdater, useEffect } from "preact/hooks";

export type Report<R> = {
    generatedAt: string;
    result: R;
};

export type Result = Failed | Passed | Errored;

export type Passed = {
    tag: "Passed";
    passedTests: Test[];
};

export type Failed = {
    tag: "Failed";
    numShrinks: number;
    reason?: string;
    passedTests: Test[];
    failedTest: Test;
};

export type Errored = {
    tag: "Errored";
    error: string;
    tests: number;
};

type Test = {
    transitions: Transition[];
}

type TestResult = "Passed" | "Failed";
type TestWithResult = Test & { result: TestResult };

type Transition = {
    actionSequence?: NonEmptyArray<Action>;
    states: { from: State, to: State };
    stutter: boolean;
};

type State = {
    screenshot: Screenshot;
    queries: Query[];
};

type Query = {
    selector: string;
    elements: QueriedElement[];
}

type Screenshot = {
    url: string;
    width: number;
    height: number;
}

type Element = {
    id: string;
    position?: Position;
};

type ActionElement = Element;
type QueriedElement = Element & { state: ElementStateValue[]; };

type ElementStateValue = {
    elementState: ElementState;
    value: any;
    diff: Diff;
};

type ElementState
    = { tag: "Attribute", name: string }
    | { tag: "Property", name: string }
    | { tag: "CssValue", name: string }
    | { tag: "Text" }
    ;

type Diff
    = "Added"
    | "Removed"
    | "Modified"
    | "Identical"
    ;

type Position = {
    width: number;
    height: number;
    x: number;
    y: number;
}

type ActionSubject = { selected: [string, number], element: ActionElement };

type Action =
    { tag: "Focus", contents: ActionSubject }
    | { tag: "KeyPress", contents: string }
    | { tag: "EnterText", contents: string }
    | { tag: "Click", contents: ActionSubject }
    | { tag: "Clear", contents: ActionSubject }
    | { tag: "Await", contents: string }
    | { tag: "AwaitWithTimeoutSecs", contents: [number, string] }
    | { tag: "Navigate", contents: string }
    | { tag: "Refresh" };

function getActionSubject(a: Action): ActionSubject[] {
    switch (a.tag) {
        case "Focus": return [a.contents];
        case "Click": return [a.contents];
        case "Clear": return [a.contents];
        case "Focus": return [a.contents];
        default: return [];
    }
}

type NonEmptyArray<T> = [T, ...T[]];

type TestViewerState = {
    test: Test,
    index: number;
    current: Transition;
};

type TestViewerAction =
    { tag: "previous" }
    | { tag: "next" }
    | { tag: "change-test", test: Test }
    ;


function testViewerReducer(state: TestViewerState, action: TestViewerAction) {
    switch (action.tag) {
        case "previous":
            return (() => {
                const newIndex = state.index - 1;
                const newCurrent = state.test.transitions[newIndex];
                if (newCurrent) {
                    return { ...state, index: newIndex, current: newCurrent };
                } else {
                    return state;
                }
            })();
        case "next":
            return (() => {
                const newIndex = state.index + 1;
                const newCurrent = state.test.transitions[newIndex];
                if (newCurrent) {
                    return { ...state, index: newIndex, current: newCurrent };
                } else {
                    return state;
                }
            })();
        case "change-test":
            return { ...state, index: 0, current: action.test.transitions[0], test: action.test };
    }
}


function TestsReport({ report }: { report: Report<Passed | Failed> }) {
    const [selectedTest, setSelectedTest] = useState<Test | null>(null);
    return <div className="report">
        <Header report={report} onTestSelect={setSelectedTest} />
        {selectedTest && <TestViewer test={selectedTest} />}
        <Footer report={report} />
    </div>;
}

function ErroredReport({ report }: { report: Report<Errored> }) {
    return <div className="report">
        <Header report={report} />
        <main>
            <section class="error">
                {report.result.error}
            </section>
        </main>
        <Footer report={report} />
    </div>;
}


function pluralize(n: number, term: string): string {
    return `${n} ${term}${n > 1 ? "s" : ""}`;
}

function ordinal(n: number): string {
    switch (n) {
        case 11: return "11th";
        case 12: return "12th";
        default:
            switch (n % 10) {
                case 1: return `${n}st`;
                case 2: return `2nd`;
                default: return `${n}th`;
            }
    }
}

function Header({ report, onTestSelect }: { report: Report<Result>, onTestSelect?: (test: Test) => void }) {
    const Summary: FunctionComponent = () => {
        switch (report.result.tag) {
            case "Failed":
                return <div class="summary failure">
                    <p>
                        Failed on {ordinal(report.result.passedTests.length + 1)} test and after {pluralize(report.result.numShrinks, "shrink")}.
              {report.result.reason}
                    </p>
                </div>;
            case "Errored":
                return <div class="summary error">
                    <p>Failed with error: {report.result.error}</p>
                </div>;
            case "Passed":
                return <div class="summary success"><p>Passed {report.result.passedTests.length} tests.</p></div>;
            default:
                return null;
        }
    }

    function annotateTest(result: TestResult): (test: Test) => TestWithResult {
        return (test => ({ ...test, result }));
    }

    function testsInResult(result: Result): TestWithResult[] {
        switch (result.tag) {
            case "Failed":
                return result.passedTests.map(annotateTest("Passed"))
                    .concat([annotateTest("Failed")(result.failedTest)]);
            case "Errored":
                return [];
            case "Passed":
                return result.passedTests.map(annotateTest("Passed"));
        }
    }

    const tests = testsInResult(report.result);
    const initial = tests[tests.length - 1];

    useEffect(() => {
        onTestSelect && onTestSelect(initial);
    }, []);

    return (
        <header>
            <div className="header-summary">
                <h1>Quickstrom Test Report</h1>
                <Summary />
            </div>
            <nav className="controls">
                {onTestSelect &&
                    <select onChange={e => onTestSelect(tests[(e.target as HTMLSelectElement).selectedIndex])}>
                        {tests.map((test, i) => (
                            <option value={i} selected={test === initial}>
                                Test {i + 1} ({test.result})
                            </option>
                        ))}
                    </select>
                }
            </nav>
        </header>
    );
}

function Footer({ report }: { report: Report<Result> }) {
    return <footer>
        Generated at <time>{report.generatedAt}</time>.
  </footer>;
}

const TestViewer: FunctionComponent<{ test: Test }> = ({ test }) => {
    const [state, dispatch] = useReducer(testViewerReducer, { current: test.transitions[0], index: 0, test });
    useEffect(() => {
        dispatch({ tag: "change-test", test });
    }, [test]);
    const [selectedElement, setSelectedElement] = useState<Element | null>(null);
    const transition = state.current;
    return <main>
        <section class="controls">
            <button disabled={state.index === 0} onClick={() => dispatch({ tag: "previous" })}>← Previous</button>
            <button disabled={state.index === (state.test.transitions.length - 1)} onClick={() => dispatch({ tag: "next" })}>Next →</button>
        </section>
        <section class="content">
            <ActionSequence actionSequence={transition.actionSequence} setSelectedElement={setSelectedElement} />
            <section class="states">
                <State number={state.index + 1} extraClass="from" label="From" />
                <State number={state.index + 2} extraClass="to" label="To" />
            </section>
            <section class="screenshots">
                <Screenshot actionSubjects={transition.actionSequence?.flatMap(getActionSubject) || []} state={transition.states.from} extraClass="from" selectedElement={selectedElement}
                    setSelectedElement={setSelectedElement} />
                <Screenshot actionSubjects={[]} state={transition.states.to} extraClass="to" selectedElement={selectedElement}
                    setSelectedElement={setSelectedElement} />
            </section>
            <section class="details">
                <div class="state-queries from">
                    <QueriesDetails queries={transition.states.from.queries} />
                </div>
                <div class="state-queries to">
                    <QueriesDetails queries={transition.states.to.queries} />
                </div>
            </section>
        </section>
    </main>
        ;
}

const ActionSequence: FunctionComponent<{ actionSequence?: NonEmptyArray<Action>, setSelectedElement: StateUpdater<Element | null> }> = ({ actionSequence, setSelectedElement }) => {
    function renderKey(key: string) {
        switch (key) {
            case "\ue006": return <span>⏎</span>;
            default: return <span>{key}</span>;
        }
    }
    function renderDetails(action: Action) {
        function renderActionSubject(subject: ActionSubject) {
            return (
                <div
                  onMouseEnter={(() => setSelectedElement(subject.element))}
                  onMouseLeave={(() => setSelectedElement(null))}
                >
                    <p class="selector">{subject.selected[0]}</p>
                    <p class="selected-index">[{subject.selected[1]}]</p>
                </div>
            );
        }
        if (!action) {
            return (
                <div class="action-details">
                    <h2><span class="name none">None</span></h2>
                </div>
            );
        }
        switch (action.tag) {
            case "Click":
                return (
                    <div class="action-details">
                        <h2><span class="name">{action.tag}</span></h2>
                        {renderActionSubject(action.contents)}
                    </div>
                );
            case "KeyPress":
                return (
                    <div class="action-details">
                        <h2><span class="name">{action.tag}</span></h2>
                        <div class="key">{renderKey(action.contents)}</div>
                    </div>
                );
            case "Clear":
                return (
                    <div class="action-details">
                        <h2><span class="name">{action.tag}</span></h2>
                        {renderActionSubject(action.contents)}
                    </div>
                );
            case "Focus":
                return (
                    <div class="action-details">
                        <h2><span class="name">{action.tag}</span></h2>
                        {renderActionSubject(action.contents)}
                    </div>
                );
            case "EnterText":
                return (
                    <div class="action-details">
                        <h2><span class="name">{action.tag}</span></h2>
                        <div className="text">{action.contents}</div>
                    </div>
                );
            case "Await":
                return (
                    <div class="action-details">
                        <h2><span class="name">{action.tag}</span></h2>
                        <p class="selector">{action.contents}</p>
                    </div>
                );
            case "AwaitWithTimeoutSecs":
                return (
                    <div class="action-details">
                        <h2><span class="name">{action.tag}</span></h2>
                        <div className="seconds">{action.contents[0]}s</div>
                        <p class="selector">{action.contents[1]}</p>
                    </div>
                );
            default:
                return (
                    <div class="action-details">
                        <h2><span class="name">{action.tag}</span></h2>
                    </div>
                );
        }
    }

    if (actionSequence) {
        return (
            <div class="action-sequence">
                <div class="action-sequence-inner">
                    <div class="label">Action Sequence</div>
                    {actionSequence.map(renderDetails)}
                </div>
            </div>
        );
    } {
        return null;
    }
}

const State: FunctionComponent<{ number: number, extraClass: string, label: string }> = ({ number, extraClass, label }) => {
    return (
        <div class={"state " + extraClass}>
            <div class=" label">{label}</div>
            <h2>State {number}</h2>
        </div>
    );
}

const MarkerDim: FunctionComponent<{ screenshot: Screenshot, element: Element | null }> = ({ screenshot, element }) => {
    if (element && element.position) {
        return (
            <svg class="marker-dim active" viewBox={`0 0 ${screenshot.width} ${screenshot.height}`}>
                <mask id={`${element.id}-mask`}>
                    <rect x="0" y="0" width={screenshot.width} height={screenshot.height} fill="white" />
                    <rect x={element.position.x} y={element.position.y} width={element.position.width}
                        height={element.position.height} fill="black" />
                </mask>
                <rect x="0" y="0" width={screenshot.width} height={screenshot.height} fill="rgba(0,0,0,.2)"
                    mask={`url(#${element.id}-mask)`} />
            </svg>
        );
    } else {
        return (
            <svg class="marker-dim inactive" viewBox={`0 0 ${screenshot.width} ${screenshot.height}`}></svg>
        );
    }
}

const Screenshot: FunctionComponent<{ actionSubjects: ActionSubject[], state: State, extraClass: string, selectedElement: Element | null, setSelectedElement: StateUpdater<Element | null> }> =
    ({ actionSubjects, state, extraClass, selectedElement, setSelectedElement }) => {
        function isActive(element: Element) {
            return selectedElement && selectedElement.id === element.id;
        }
        const activeElement = 
            state.queries.flatMap(q => q.elements as Element[])
                .concat(actionSubjects.map(a => a.element))
                .find(isActive) 
                || null;

        function renderDim(element: Element | null) {
            return (
                <MarkerDim screenshot={state.screenshot} element={element} />
            );
        }
        function percentageOf(x: number, total: number): string {
            return `${(x / total) * 100}%`;
        }
        function renderQueryMarkers(query: Query) {
            return query.elements.map(element => {
                if (element.position) {
                    return (
                        <div key={element.id}
                            className={`marker ${isActive(element) ? " active" : "inactive"}`}
                            onMouseEnter={(() => setSelectedElement(element))}
                            onMouseLeave={(() => setSelectedElement(null))}
                            style={{
                                top: percentageOf(element.position.y, state.screenshot.height),
                                left: percentageOf(element.position.x, state.screenshot.width),
                                width: percentageOf(element.position.width, state.screenshot.width),
                                height: percentageOf(element.position.height, state.screenshot.height)
                            }}>
                            < div class="marker-details" >
                                <ElementStateTable element={element} />
                            </div >
                        </div >
                    );
                }
            });
        }
        const dim = renderDim(activeElement);
        return (
            <div class={`state-screenshot ${extraClass}`}>
                <div class=" state-screenshot-inner">
                    {state.queries.map(renderQueryMarkers)}
                    <img src={state.screenshot.url} />
                    {dim}
                </div>
            </div>
        );
    }


const QueriesDetails: FunctionComponent<{ queries: Query[] }> = ({ queries }) => {
    return <ul class="queries-details">
        {queries.map(query =>
            <li>
                <h2 class="selector">{query.selector}</h2>
                <QueryDetails elements={query.elements} />
            </li>
        )}
    </ul>;
};

const QueryDetails: FunctionComponent<{ elements: QueriedElement[] }> = ({ elements }) => {
    return <ul>
        {elements.map(element =>
            <li>
                <h3>{element.id}</h3>
                <ElementStateTable element={element} />
            </li>
        )}
    </ul>;
};
const ElementStateTable: FunctionComponent<{ element: QueriedElement }> = ({ element }) => {
    function elementStateName(s: ElementState): string {
        switch (s.tag) {
            case "Text":
                return "Text";
            default:
                return s.name;
        }
    }
    return <table class="element-state">
        {element.state.map(e => (
            <tr class={e.diff.toLowerCase()}>
                <td>{elementStateName(e.elementState)}</td>
                <td>{e.value}</td>
            </tr>
        ))}
    </table>;
};

function excludeStutters(report: Report<Failed>): Report<Failed> {
    return {
        ...report,
        result: {
            ...report.result,
            failedTest: {
                transitions: report.result.failedTest.transitions.filter(t => !t.stutter || !!t.actionSequence)
            },
        },
    };
}

function App({ report }: { report: Report<Result> }) {
    switch (report.result.tag) {
        case "Passed":
            return <TestsReport report={report as Report<Passed>} />;
        case "Errored":
            return <ErroredReport report={report as Report<Errored>} />;
        case "Failed":
            return <TestsReport report={excludeStutters(report as Report<Failed>)} />;
    }
}

export default App;
