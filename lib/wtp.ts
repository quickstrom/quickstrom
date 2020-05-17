// ESSENTIALS

// https://stackoverflow.com/a/25456134
function deepEqual(x: any, y: any) {
  if (x === y) {
    return true;
  } else if (
    typeof x == "object" &&
    x != null &&
    typeof y == "object" &&
    y != null
  ) {
    if (Object.keys(x).length != Object.keys(y).length) return false;

    for (var prop in x) {
      if (y.hasOwnProperty(prop)) {
        if (!deepEqual(x[prop], y[prop])) return false;
      } else return false;
    }

    return true;
  } else return false;
}

function toArray<T, A extends { [index: number]: T }>(xs: A): T[] {
  return Array.prototype.slice.call(xs);
}

type Optional<T> = T | null;

function isNotNull<T>(x: Optional<T>): x is T {
  return x !== null;
}

type NonEmptyArray<T> = [T, ...T[]];

function isNonEmpty<T>(arr: T[]): arr is NonEmptyArray<T> {
  return arr.length > 0;
}

// QUERIES

type Selector = string;

type ElementQuery = { tag: "element"; selector: Selector };

type AttributeQuery = { tag: "attribute"; name: string };
type PropertyQuery = { tag: "property"; name: string };
type CssValueQuery = { tag: "cssValue"; name: string };
type TextQuery = { tag: "text" };
type EnabledQuery = { tag: "enabled" };

type StateQuery =
  | AttributeQuery
  | PropertyQuery
  | CssValueQuery
  | TextQuery
  | EnabledQuery;

type ElementStateQuery = {
  tag: "elementState";
  elementQuery: ElementQuery;
  stateQuery: StateQuery;
};

type Query = ElementQuery | ElementStateQuery;

function selectorInQuery(query: Query): Selector {
  switch (query.tag) {
    case "element":
      return query.selector;
    case "elementState":
      return query.elementQuery.selector;
  }
}

type Value = string | object | number | boolean | Element | null;

function runQuery(query: Query): Array<Value> {
  function runStateQuery(element: Element, stateQuery: StateQuery): Value {
    switch (stateQuery.tag) {
      case "attribute":
        return element.attributes.getNamedItem(stateQuery.name);
      case "property":
        // @ts-ignore
        return element[stateQuery.name];
      case "cssValue":
        return window
          .getComputedStyle(element)
          .getPropertyValue(stateQuery.name);
      case "text":
        return element.textContent;
      case "enabled":
        return (
          (element instanceof HTMLButtonElement ||
            element instanceof HTMLInputElement) &&
          element.disabled === false
        );
    }
  }
  switch (query.tag) {
    case "element":
      return Array.prototype.slice.call(
        document.querySelectorAll(query.selector)
      );
    case "elementState":
      return runQuery(query.elementQuery).map((element) =>
        runStateQuery(element as Element, query.stateQuery)
      );
  }
}

function renderQuery(query: Query): string {
  function renderStateQuery(stateQuery: StateQuery): string {
    switch (stateQuery.tag) {
      case "attribute":
        return `attribute ${stateQuery.name}`;
      case "property":
        return `property ${stateQuery.name}`;
      case "cssValue":
        return `css ${stateQuery.name}`;
      case "text":
        return "text";
      case "enabled":
        return "enabled";
    }
  }
  switch (query.tag) {
    case "element":
      return query.selector;
    case "elementState":
      return `${renderQuery(query.elementQuery)} -> ${renderStateQuery(
        query.stateQuery
      )}`;
  }
}

// ACTIONS

type ClickAction<T> = { tag: "click"; target: T };

type WaitAction<T> = { tag: "wait"; duration: T };

type SelectedElement = { selector: Selector; index: number };

type Action = ClickAction<Selector> | WaitAction<[number, number]>;

type SelectedAction = ClickAction<SelectedElement> | WaitAction<number>;

function renderAction(action: SelectedAction): string {
  switch (action.tag) {
    case "click":
      return `click ${action.target.selector}[${action.target.index}]`;
    case "wait":
      return `wait ${action.duration}ms`;
  }
}

function pickRandom<A>(xs: NonEmptyArray<A>): A {
  return xs[Math.floor(Math.random() * xs.length)];
}

function pickBetween(min: number, max: number): number {
  return min + Math.random() * (max - min);
}

function selectAction(action: Action): Optional<SelectedAction> {
  switch (action.tag) {
    case "click":
      const els = Array.prototype.slice
        .call(document.querySelectorAll(action.target))
        .map((el, i) => [el, i])
        .filter(
          ([el]) => isElementVisible(el) && !(el as HTMLButtonElement).disabled
        );
      if (isNonEmpty(els)) {
        const selectedElement = pickRandom(els);
        return {
          tag: action.tag,
          target: { selector: action.target, index: selectedElement[1] },
        };
      } else {
        return null;
      }
    case "wait":
      const duration = pickBetween(action.duration[0], action.duration[1]);
      return { tag: "wait", duration };
  }
}

function selectNextAction(actions: Array<Action>): Optional<SelectedAction> {
  const validActions: Array<SelectedAction> = actions
    .map(selectAction)
    .filter(isNotNull);
  if (isNonEmpty(validActions)) {
    return pickRandom(validActions);
  } else {
    return null;
  }
}

function delay(ms: number): Promise<void> {
  return new Promise((resolve) => {
    setTimeout(resolve, ms);
  });
}

async function runAction(action: SelectedAction): Promise<void> {
  switch (action.tag) {
    case "click":
      const el = document.querySelectorAll(action.target.selector)[
        action.target.index
      ];
      (el as HTMLElement).click();
      return;
    case "wait":
      return delay(action.duration);
  }
}

// FRONTEND LANGUAGE

export function byCss(selector: Selector): ElementQuery {
  return { tag: "element", selector };
}

export function text(elementQuery: ElementQuery): ElementStateQuery {
  return { tag: "elementState", stateQuery: { tag: "text" }, elementQuery };
}

export function enabled(elementQuery: ElementQuery): ElementStateQuery {
  return { tag: "elementState", stateQuery: { tag: "enabled" }, elementQuery };
}

export function click(target: Selector): ClickAction<Selector> {
  return { tag: "click", target };
}

export function wait(min: number, max: number): WaitAction<[number, number]> {
  return { tag: "wait", duration: [min, max] };
}

// OBSERVATION

type ObservedState = Map<Query, Value[]>;

function mergeStates(states: ObservedState[]): ObservedState {
  const s = new Map();
  states.forEach((state) => {
    state.forEach((value, key) => s.set(key, value));
  });
  return s;
}

function isStateEqual(map1: ObservedState, map2: ObservedState): boolean {
  var testVal;
  if (map1.size !== map2.size) {
    return false;
  }
  for (var [key, val] of map1) {
    testVal = map2.get(key);
    // console.log(val, testVal);
    // in cases of an undefined value, make sure the key
    // actually exists on the object so there are no false positives
    if (!deepEqual(testVal, val) || (testVal === undefined && !map2.get(key))) {
      return false;
    }
  }
  return true;
}

function logObservedState(label: string, s: ObservedState) {
  console.info(label);
  s.forEach((values, query) => {
    console.log(" *", renderQuery(query));
    values.forEach((value) => console.log("   -", value));
  });
}

function observeInitialStates(queries: Query[]): ObservedState {
  const m = new Map();
  queries.forEach((query) => m.set(query, runQuery(query)));
  return m;
}

async function observeNextStateForQuery<Q extends Query>(
  query: Q
): Promise<ObservedState> {
  const selector = selectorInQuery(query);

  function queryMatching(node: Node): Optional<Value> {
    if (node instanceof Element && node.matches(selector)) {
      return runQuery(query);
    } else {
      return null;
    }
  }

  return new Promise((resolve) => {
    new MutationObserver((mutations, observer) => {
      const updatedQueries: Value[] = mutations.flatMap((mutation) => {
        return [
          [mutation.target],
          toArray(mutation.addedNodes) as Node[],
          toArray(mutation.removedNodes) as Node[],
        ]
          .flat()
          .flatMap(queryMatching)
          .filter(isNotNull);
      });
      const m = new Map<Query, Value[]>();
      if (updatedQueries.length > 0) {
        m.set(query, updatedQueries);
      }
      observer.disconnect();
      resolve(m);
    }).observe(document, { childList: true, subtree: true, attributes: true });
  });
}

async function observeNextState<Q extends Query>(
  queries: Q[]
): Promise<ObservedState> {
  const allStates = await Promise.all(queries.map(observeNextStateForQuery));
  return mergeStates(allStates);
}

async function observeNextNonStutterState<Q extends Query>(
  previous: ObservedState,
  queries: Q[]
): Promise<ObservedState> {
  var nextState;
  do {
    nextState = mergeStates([previous, await observeNextState(queries)]);
  } while (isStateEqual(previous, nextState));
  return nextState;
}

type Specification = {
  actions: Action[];
  queries: Query[];
  maxActions?: number;
  actionTimeout?: number;
};

async function runAndObserveAction(
  spec: Specification,
  currentState: ObservedState,
  action: SelectedAction,
  n: number
): Promise<Optional<ObservedState>> {
  const changedState = observeNextNonStutterState(currentState, spec.queries);
  // console.info(`Running action #${n}: ${renderAction(action)}`);
  await runAction(action);
  const newState = await Promise.race([
    changedState,
    delay(spec.actionTimeout || 1000).then(() => currentState),
  ]);
  let merged = mergeStates([currentState, newState]);
  return isStateEqual(currentState, merged) ? null : merged;
}

async function runNext(
  spec: Specification,
  n: number,
  currentState: ObservedState
): Promise<void> {
  if (n <= (spec.maxActions || 100)) {
    const selected = selectNextAction(spec.actions);
    if (selected) {
      const newState = await runAndObserveAction(spec, currentState, selected, n);
      if (newState) {
        logObservedState(`Update on action ${n}:`, newState);
        await runNext(spec, n + 1, newState);
      } else {
        console.warn("Stutter...");
        await runNext(spec, n + 1, currentState);
      }
    } else {
      logObservedState("No more valid actions. Terminal state:", currentState);
    }
  } else {
    logObservedState("Ran maxmimum number of actions. Terminal state:", currentState);
  }
}

export function check(spec: Specification) {
  const initial = observeInitialStates(spec.queries);
  logObservedState("Initial:", initial);
  runNext(spec, 1, initial);
}

// OLD STUFF

export function awaitElement(sel: string, done: () => void) {
  var timer = setInterval(function () {
    if (document.querySelector(sel)) {
      clearInterval(timer);
      done();
    }
  }, 100);
}

export function isElementVisible(el: HTMLElement): boolean {
  const cs = window.getComputedStyle(el);
  return (
    cs.getPropertyValue("display") !== "none" &&
    cs.getPropertyValue("visibility") !== "hidden" &&
    cs.getPropertyValue("opacity") !== "0" &&
    el.offsetParent !== null
  );
}
