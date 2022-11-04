import { h, FunctionComponent } from "preact";
import Preact from "preact";
import { useReducer, useState, StateUpdater, useEffect } from "preact/hooks";
import { keyName } from "./keys";

export type Report<R> = {
  tag: "Report";
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
  passedTests: Test[];
  failedTest: Test;
};

export type Errored = {
  tag: "Errored";
  passedTests: Test[];
  erroredTest: Test;
  message: string;
};

type Test = {
  validity: Validity;
  transitions: Transition[];
};

type Certainty = "Definitely" | "Probably";

type Validity = {
  certainty: Certainty;
  value: boolean;
};

type StateTransition = {
  tag: "StateTransition";
  actions: NonEmptyArray<Action>;
  fromState?: State;
  toState: State;
};
type ErrorTransition = {
  tag: "ErrorTransition";
  actions: NonEmptyArray<Action>;
  fromState?: State;
  error: string;
};

type Transition = StateTransition | ErrorTransition;

type State = {
  screenshot: Screenshot;
  queries: Queries;
};

type Queries = { [selector: string]: QueriedElement[] };

function uniqueElementsInState(state: State): {
  [ref: string]: QueriedElement;
} {
  const out: { [ref: string]: QueriedElement } = {};
  Object.values(state.queries)
    .flatMap((elements) => elements)
    .forEach((element) => {
      out[element.ref] = element;
    });
  return out;
}

type Screenshot = {
  url: string;
  width: number;
  height: number;
  scale: number;
};

function scaled(s: Screenshot): Screenshot {
  const scale = (n: number) => Math.round(n / s.scale);
  return { ...s, width: scale(s.width), height: scale(s.height) };
}

type Element = {
  ref: string;
  position?: Position;
  diff: Diff;
};

type ActionElement = Element;
type QueriedElement = Element;

type Diff = "Added" | "Removed" | "Modified" | "Unmodified";

type Position = {
  width: number;
  height: number;
  x: number;
  y: number;
};

type Action = {
  id: string;
  isEvent: boolean;
  args: Array<any>;
  timeout?: number;
};

type NonEmptyArray<T> = [T, ...T[]];

type TestViewerState = {
  test: Test;
  index: number;
  current: Transition;
};

type TestViewerAction =
  | { tag: "first" }
  | { tag: "last" }
  | { tag: "previous" }
  | { tag: "next" }
  | { tag: "change-test"; test: Test };

function testViewerReducer(
  state: TestViewerState,
  action: TestViewerAction
): TestViewerState {
  switch (action.tag) {
    case "first":
      return (() => {
        const newIndex = 0;
        const newCurrent = state.test.transitions[newIndex];
        if (newCurrent) {
          return { ...state, index: newIndex, current: newCurrent };
        } else {
          return state;
        }
      })();
    case "last":
      return (() => {
        const newIndex = state.test.transitions.length - 1;
        const newCurrent = state.test.transitions[newIndex];
        function lastState() {
          const lastTransition = state.test.transitions[newIndex - 1];
          if (lastTransition && lastTransition.tag === "StateTransition") {
            return lastTransition.toState;
          }
        }
        if (newCurrent) {
          return {
            ...state,
            index: newIndex,
            current: newCurrent,
            lastState: lastState(),
          };
        } else {
          return state;
        }
      })();
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
        function lastState() {
          const lastTransition = state.test.transitions[newIndex - 1];
          if (lastTransition && lastTransition.tag === "StateTransition") {
            return lastTransition.toState;
          }
        }
        if (newCurrent) {
          return {
            ...state,
            index: newIndex,
            current: newCurrent,
            lastState: lastState(),
          };
        } else {
          return state;
        }
      })();
    case "change-test":
      return {
        ...state,
        index: 0,
        current: action.test.transitions[0],
        test: action.test,
      };
  }
}

function TestsReport({ report }: { report: Report<Passed | Failed> }) {
  const [selectedTest, setSelectedTest] = useState<Test | null>(null);
  return (
    <div className="report">
      <Header report={report} onTestSelect={setSelectedTest} />
      {selectedTest && <TestViewer test={selectedTest} />}
      <Footer report={report} />
    </div>
  );
}

function ErroredReport({ report }: { report: Report<Errored> }) {
  return (
    <div className="report">
      <Header report={report} />
      <section class="error">{report.result.message}</section>
      {<TestViewer test={report.result.erroredTest} />}
      <Footer report={report} />
    </div>
  );
}

function ordinal(n: number): string {
  switch (n) {
    case 11:
      return "11th";
    case 12:
      return "12th";
    default:
      switch (n % 10) {
        case 1:
          return `${n}st`;
        case 2:
          return `2nd`;
        default:
          return `${n}th`;
      }
  }
}

function Header({
  report,
  onTestSelect,
}: {
  report: Report<Result>;
  onTestSelect?: (test: Test) => void;
}) {
  const Summary: FunctionComponent = () => {
    switch (report.result.tag) {
      case "Failed":
        return (
          <div class="summary failure">
            <p>
              Failed on {ordinal(report.result.passedTests.length + 1)} test.
            </p>
          </div>
        );
      case "Errored":
        return (
          <div class="summary error">
            <p>Errored!</p>
          </div>
        );
      case "Passed":
        return (
          <div class="summary success">
            <p>Passed {report.result.passedTests.length} tests.</p>
          </div>
        );
      default:
        return null;
    }
  };

  function testsInResult(result: Result): Test[] {
    switch (result.tag) {
      case "Failed":
        return result.passedTests.concat([result.failedTest]);
      case "Errored":
        return [];
      case "Passed":
        return result.passedTests;
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
        {onTestSelect && (
          <select
            onChange={(e) =>
              onTestSelect(tests[(e.target as HTMLSelectElement).selectedIndex])
            }
          >
            {tests.map((test, i) => (
              <option value={i} selected={test === initial}>
                Test {i + 1} ({test.validity.certainty}{" "}
                {test.validity.value.toString()})
              </option>
            ))}
          </select>
        )}
      </nav>
    </header>
  );
}

function Footer({ report }: { report: Report<Result> }) {
  return (
    <footer>
      Generated at <time>{report.generatedAt}</time>.
    </footer>
  );
}

const TestViewer: FunctionComponent<{ test: Test }> = ({ test }) => {
  const [state, dispatch] = useReducer(testViewerReducer, {
    current: test.transitions[0],
    index: 0,
    test,
  });
  useEffect(() => {
    dispatch({ tag: "change-test", test });
  }, [test]);
  const [selectedElement, setSelectedElement] = useState<Element | null>(null);
  const transition = state.current;

  function transitionToDetails(transition: Transition) {
    if (transition.tag === "StateTransition") {
      return Object.keys(transition.toState.queries).map((selector) => {
        return (
          <div class="query">
            <h2 class="selector">`{selector}`</h2>
            <div class="state-queries from">
              {transition.fromState && (
                <QueryDetails
                  elements={transition.fromState.queries[selector]}
                />
              )}
            </div>
            <div class="state-queries to">
              <QueryDetails elements={transition.toState.queries[selector]} />
            </div>
          </div>
        );
      });
    } else {
      return <section className="error">{transition.error}</section>;
    }
  }
  return (
    <main>
      <section class="controls">
        <nav className="backwards">
          <button
            disabled={state.index === 0}
            onClick={() => dispatch({ tag: "first" })}
          >
            ⇤ First
          </button>
          <button
            disabled={state.index === 0}
            onClick={() => dispatch({ tag: "previous" })}
          >
            ← Previous
          </button>
        </nav>
        <nav className="forwards">
          <button
            disabled={state.index === state.test.transitions.length - 1}
            onClick={() => dispatch({ tag: "next" })}
          >
            Next →
          </button>
          <button
            disabled={state.index === state.test.transitions.length - 1}
            onClick={() => dispatch({ tag: "last" })}
          >
            Last ⇥
          </button>
        </nav>
      </section>
      <section class="content">
        <Actions
          initial={state.index === 0}
          actions={transition.actions}
          setSelectedElement={setSelectedElement}
        />
        <section class="states">
          <State number={state.index} extraClass="from" label="From" />
          <State number={state.index + 1} extraClass="to" label="To" />
        </section>
        <section class="screenshots">
          {state.current.fromState?.screenshot ? (
            <Screenshot
              state={state.current.fromState}
              extraClass="from"
              selectedElement={selectedElement}
              setSelectedElement={setSelectedElement}
            />
          ) : (
            <MissingScreenshot />
          )}
          {transition.tag === "StateTransition" &&
          transition.toState?.screenshot ? (
            <Screenshot
              state={transition.toState}
              extraClass="to"
              selectedElement={selectedElement}
              setSelectedElement={setSelectedElement}
            />
          ) : (
            <MissingScreenshot />
          )}
        </section>
        <section class="details">{transitionToDetails(state.current)}</section>
      </section>
    </main>
  );
};

const Actions: FunctionComponent<{
  initial: boolean;
  actions: NonEmptyArray<Action>;
  setSelectedElement: StateUpdater<Element | null>;
}> = ({ initial, actions, setSelectedElement }) => {
  function renderArg(arg: any): string {
    return keyName(arg) || JSON.stringify(arg);
  }
  function renderTimeoutSuffix(action: Action) {
        return action.timeout ? ` timeout ${action.timeout}` : null;
  }
  function renderDetails(action: Action) {
    // <div
    //   onMouseEnter={() => setSelectedElement(subject.element)}
    //   onMouseLeave={() => setSelectedElement(null)}
    // >
    return (
      <div class="action-details">
        <code>
          <span class="id">{action.id}</span>(
          {action.args.map(renderArg).join(", ")}){renderTimeoutSuffix(action)}
        </code>
      </div>
    );
  }

  return (
    <div class={`actions ${initial ? "initial" : ""}`}>
      <div class="actions-inner">
        <div class="label">Actions & Events</div>
        {actions.map(renderDetails)}
      </div>
    </div>
  );
};

const State: FunctionComponent<{
  number: number;
  extraClass: string;
  label: string;
}> = ({ number, extraClass, label }) => {
  if (number > 0) {
    return (
      <div class={"state " + extraClass}>
        <div class=" label">{label}</div>
        <h2>State {number}</h2>
      </div>
    );
  } else {
    return <div class={"state"} />;
  }
};

const MarkerDim: FunctionComponent<{
  screenshot: Screenshot;
  element: Element | null;
}> = ({ screenshot, element }) => {
  const s = scaled(screenshot);
  if (element && element.position) {
    return (
      <svg class="marker-dim active" viewBox={`0 0 ${s.width} ${s.height}`}>
        <mask id={`${element.ref}-mask`}>
          <rect x="0" y="0" width={s.width} height={s.height} fill="white" />
          <rect
            x={element.position.x}
            y={element.position.y}
            width={element.position.width}
            height={element.position.height}
            fill="black"
          />
        </mask>
        <rect
          x="0"
          y="0"
          width={s.width}
          height={s.height}
          fill="rgba(0,0,0,.2)"
          mask={`url(#${element.ref}-mask)`}
        />
      </svg>
    );
  } else {
    return (
      <svg
        class="marker-dim inactive"
        viewBox={`0 0 ${screenshot.width} ${screenshot.height}`}
      ></svg>
    );
  }
};

const MissingScreenshot: FunctionComponent = () => {
  return (
    <div class={`state-screenshot missing`}>
      <div class=" state-screenshot-inner"></div>
    </div>
  );
};

const Screenshot: FunctionComponent<{
  state: State;
  extraClass: string;
  selectedElement: Element | null;
  setSelectedElement: StateUpdater<Element | null>;
}> = ({ state, extraClass, selectedElement, setSelectedElement }) => {
  function isActive(element: Element) {
    return selectedElement && selectedElement.ref === element.ref;
  }
  const activeElement =
    Object.values(state.queries)
      .flatMap((q) => q as Element[])
      .find(isActive) || null;

  function renderDim(element: Element | null) {
    return <MarkerDim screenshot={state.screenshot} element={element} />;
  }
  function percentageOf(x: number, total: number): string {
    return `${(x / total) * 100}%`;
  }
  function renderQueryMarkers(element: QueriedElement) {
    if (element.position && state.screenshot) {
      const s = scaled(state.screenshot);
      return (
        <div
          key={element.ref}
          className={`marker ${isActive(element) ? " active" : "inactive"}`}
          onMouseEnter={() => setSelectedElement(element)}
          onMouseLeave={() => setSelectedElement(null)}
          style={{
            top: percentageOf(element.position.y, s.height),
            left: percentageOf(element.position.x, s.width),
            width: percentageOf(element.position.width, s.width),
            height: percentageOf(element.position.height, s.height),
          }}
        >
          <div class="marker-details">
            <ElementState element={element} />
          </div>
        </div>
      );
    }
  }
  const dim = renderDim(activeElement);
  const s = scaled(state.screenshot);
  return (
    <div class={`state-screenshot ${extraClass}`}>
      <div class=" state-screenshot-inner">
        {Object.values(uniqueElementsInState(state)).map(renderQueryMarkers)}
        <img src={s.url} width={s.width} height={s.height} />
        {dim}
      </div>
    </div>
  );
};

const QueryDetails: FunctionComponent<{ elements: QueriedElement[] }> = ({
  elements,
}) => {
  return (
    <ul>
      {elements.map((element) => (
        <li>
          <ElementState element={element} />
        </li>
      ))}
    </ul>
  );
};
const ElementState: FunctionComponent<{ element: QueriedElement }> = ({
  element,
}) => {
  const ignoredKeys = ["ref", "diff", "position"];
  return (
    <div class={"element-state " + (element.diff?.toLowerCase() || "")}>
      <table>
        <thead>
          <tr>
            <th colSpan={2}>{element.ref}</th>
          </tr>
        </thead>
        {Object.entries(element)
          .filter(([k, _]) => ignoredKeys.indexOf(k) < 0)
          .map(([key, value]) => (
            <tr>
              <td>{key}</td>
              <td>{JSON.stringify(value)}</td>
            </tr>
          ))}
      </table>
    </div>
  );
};

function App({ report }: { report: Report<Result> }) {
  switch (report.result.tag) {
    case "Passed":
      return <TestsReport report={report as Report<Passed>} />;
    case "Errored":
      return <ErroredReport report={report as Report<Errored>} />;
    case "Failed":
      return <TestsReport report={report as Report<Failed>} />;
  }
}

export default App;
