import { h, FunctionComponent } from 'preact';
import Preact from 'preact';
import { useReducer, useState, StateUpdater } from "preact/hooks";

type Report<R> = {
    generatedAt: string;
    result: R;
};

type Result = Failed | Passed | Errored;

type Passed = {
    tag: "Passed";
    passedTests: Test[];
};

type Failed = {
    tag: "Failed";
    shrinkLevels: number;
    reason?: string;
    passedTests: Test[];
    failedTest: Test;
};

type Errored = {
    tag: "Errored";
    error: string;
    tests: number;
};

type Test = {
    transitions: Transition[];
}

type Transition = {
    action?: Action;
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

type QueriedElement = {
    id: string;
    position: Position;
    state: ElementStateValue[];
};

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

type Action =
    { tag: "Focus", contents: [string, number] }
    | { tag: "KeyPress", contents: string }
    | { tag: "EnterText", contents: string }
    | { tag: "Click", contents: [string, number] }
    | { tag: "Navigate", contents: string };

type AppState<R> = {
    report: Report<R>,
    index: number;
    current: Transition;
};

type UserAction =
    { tag: "previous" }
    | { tag: "next" }
    ;


function reportReducer(state: AppState<Failed>, action: UserAction) {
    if (action.tag === "previous") {
        const newIndex = state.index - 1;
        const newCurrent = state.report.result.failedTest.transitions[newIndex];
        if (newCurrent) {
            return { ...state, index: newIndex, current: newCurrent };
        } else {
            return state;
        }
    } else if (action.tag === "next") {
        const newIndex = state.index + 1;
        const newCurrent = state.report.result.failedTest.transitions[newIndex];
        if (newCurrent) {
            return { ...state, index: newIndex, current: newCurrent };
        } else {
            return state;
        }
    } else {
        return state;
    }
}


function PassedReport({ report }: { report: Report<Passed> }) {
    return <div className="report">
        <Header report={report} />
        <main />
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


function FailedReport({ report }: { report: Report<Failed> }) {
    const [state, dispatch] = useReducer(reportReducer, { current: report.result.failedTest.transitions[0], index: 0, report });
    return <div className="report">
        <Header report={report} />
        <Transition state={state} dispatch={dispatch} />
        <Footer report={report} />
    </div>;
}

function pluralize(n: number, term: string): string {
    return `${n} ${term}${n > 1 ? "s" : ""}`;
}

function cardinalize(n: number): string {
    switch (n) {
        case 11: return "11th";
        case 12: return "12th";
        default:
            switch (n % 10) {
                case 1: return `${n}st`;
                case 2: return `2nd`;
                default: return `1st`;
            }
    }
}

function Header({ report }: { report: Report<Result> }) {
    const Summary: FunctionComponent = () => {
        switch (report.result.tag) {
            case "Failed":
                return <div class="summary failure">
                    <p>
                        Failed on {cardinalize(report.result.passedTests.length + 1)} test and {pluralize(report.result.shrinkLevels, "level")} of
              shrinking
              {report.result.reason || "."}
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

    return <header>
        <h1>Quickstrom Test Report</h1>
        <Summary />
    </header>;
}

function Footer({ report }: { report: Report<Result> }) {
    return <footer>
        Generated at <time>{report.generatedAt}</time>.
  </footer>;
}

const Transition: FunctionComponent<{ state: AppState<Failed>, dispatch: (action: UserAction) => void }> = ({ state, dispatch }) => {
    const [selectedElement, setSelectedElement] = useState<QueriedElement | null>(null);
    const transition = state.current;
    return <main>
        <section class="controls">
            <button disabled={state.index === 0} onClick={() => dispatch({ tag: "previous" })}>←</button>
        </section>
        <section class="content">
            <Action action={transition.action} />
            <section class="states">
                <State number={state.index + 1} extraClass="from" label="From" />
                <State number={state.index + 2} extraClass="to" label="To" />
            </section>
            <section class="screenshots">
                <Screenshot state={transition.states.from} extraClass="from" selectedElement={selectedElement}
                    setSelectedElement={setSelectedElement} />
                <Screenshot state={transition.states.to} extraClass="to" selectedElement={selectedElement}
                    setSelectedElement={setSelectedElement} />
            </section>
            <section class="details">
                <div class="state-queries from">
                </div>
                <div class="state-queries to">
                </div>
            </section>
        </section>
        <section class="controls">
            <button disabled={state.index === (state.report.result.failedTest.transitions.length - 1)} onClick={() => dispatch({ tag: "next" })}>→</button>
        </section>
    </main>
        ;
}

const Action: FunctionComponent<{ action?: Action }> = ({ action }) => {
    function renderKey(key: string) {
        switch (key) {
            case "\ue006": return <span>⏎</span>;
            default: return <span>{key}</span>;
        }
    }
    function renderDetails() {
        function selector(sel: [string, number]) {
            return (
                <div>
                    <p class="selector">{sel[0]}</p>
                    <p class="selected-index">[{sel[1]}]</p>
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
                        {selector(action.contents)}
                    </div>
                );
            case "KeyPress":
                return (
                    <div class="action-details">
                        <h2><span class="name">{action.tag}</span></h2>
                        <div class="key">{renderKey(action.contents)}</div>
                    </div>
                );
            case "Focus":
                return (
                    <div class="action-details">
                        <h2><span class="name">{action.tag}</span></h2>
                        {selector(action.contents)}
                    </div>
                );
        }
    }

    return (
        <section class="action">
            <div class="action-inner">
                <div class="label">Action</div>
                {renderDetails()}
            </div>
        </section>
    );
}

const State: FunctionComponent<{ number: number, extraClass: string, label: string }> = ({ number, extraClass, label }) => {
    return (
        <div class={"state " + extraClass}>
            <div class=" label">{label}</div>
            <h2>State {number}</h2>
        </div>
    );
}

const MarkerDim: FunctionComponent<{ screenshot: Screenshot, element: QueriedElement | null }> = ({ screenshot, element }) => {
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

const Screenshot: FunctionComponent<{ state: State, extraClass: string, selectedElement: QueriedElement | null, setSelectedElement: StateUpdater<QueriedElement | null> }> =
    ({ state, extraClass, selectedElement, setSelectedElement }) => {
        function isActive(element: QueriedElement) {
            return selectedElement && selectedElement.id === element.id;
        }
        const activeElement = state.queries.flatMap(q => q.elements).find(isActive) || null;

        function renderDim(element: QueriedElement | null) {
            return (
                <MarkerDim screenshot={state.screenshot} element={element} />
            );
        }
        function percentageOf(x: number, total: number): string {
            return `${(x / total) * 100}%`;
        }
        function elementStateName(s: ElementState): string {
            switch (s.tag) {
                case "Text":
                    return "Text";
                default:
                    return s.name;
            }
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
                                <table>
                                    {element.state.map(e => (
                                        <tr class={e.diff.toLowerCase()}>
                                            <td>{elementStateName(e.elementState)}</td>
                                            <td>{e.value}</td>
                                        </tr>
                                    ))}
                                </table>
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

function excludeStutters(report: Report<Failed>): Report<Failed> {
    return {
        ...report,
        result: {
            ...report.result,
            failedTest: {
                transitions: report.result.failedTest.transitions.filter(t => !t.stutter || !!t.action)
            },
        },
    };
}

function App() {
    const report: Report<Result> = 
        // @ts-ignore
        window.report;

    switch (report.result.tag) {
        case "Passed":
            return <PassedReport report={report as Report<Passed>} />;
        case "Errored":
            return <ErroredReport report={report as Report<Errored>} />;
        case "Failed":
            return <FailedReport report={excludeStutters(report as Report<Failed>)} />;
    }
}

export default App;
