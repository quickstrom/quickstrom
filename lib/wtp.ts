// ESSENTIALS

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
      return `${renderQuery(query.elementQuery)} -> ${renderStateQuery(query.stateQuery)}`;
  }
}


// ACTIONS

type ClickAction<T> = { tag: "click"; target: T };

type WaitAction<T> = { tag: "wait"; duration: T };

type SelectedElement = { selector: Selector; index: number };

type Action = ClickAction<Selector> | WaitAction<[number, number]>;

type SelectedAction = ClickAction<SelectedElement> | WaitAction<number>;

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
        .filter(([el]) => isElementVisible(el));
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

function selectNextAction(actions: Array<Action>): SelectedAction {
  const validActions: Array<SelectedAction> = actions
    .map(selectAction)
    .filter(isNotNull);
  if (isNonEmpty(validActions)) {
    return pickRandom(validActions);
  } else {
    return { tag: "wait", duration: 100 };
  }
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
      return new Promise((resolve) => {
        setTimeout(resolve, action.duration);
      });
  }
}

// FRONTEND LANGUAGE

function byCss(selector: Selector): ElementQuery {
  return { tag: "element", selector };
}

function text(elementQuery: ElementQuery): ElementStateQuery {
  return { tag: "elementState", stateQuery: { tag: "text" }, elementQuery };
}

function enabled(elementQuery: ElementQuery): ElementStateQuery {
  return { tag: "elementState", stateQuery: { tag: "enabled" }, elementQuery };
}

function click(target: Selector): ClickAction<Selector> {
  return { tag: "click", target };
}

function wait(min: number, max: number): WaitAction<[number, number]> {
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

function logObservedState(label: string, s: ObservedState) {
  console.log(label);
  s.forEach((values, query) => {
    console.log(" *", renderQuery(query));
    values.forEach(value => console.log("   -", value));
  });
}

function observeInitialStates(queries: Query[]): ObservedState {
    const m = new Map();
    queries.forEach(query => m.set(query, runQuery(query)))
    return m;
}

async function observeQuery<Q extends Query>(query: Q): Promise<ObservedState> {
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
      if (updatedQueries.length > 0) { m.set(query, updatedQueries); }
      observer.disconnect();
      resolve(m);
    }).observe(document, { childList: true, subtree: true, attributes: true });
  });
}

async function observeQueries<Q extends Query>(
  queries: Q[]
): Promise<ObservedState> {
  const allStates = await Promise.all(queries.map(observeQuery));
  return mergeStates(allStates);
}

// DEMO

const queries = [enabled(byCss("button")), text(byCss(".message"))];
const actions = [click("button")];

async function runNext(n: number, acc: ObservedState) {
  if (n > 0) {
    const selected = selectNextAction(actions);
    const sp = observeQueries(queries);
    await runAction(selected);
    const newState = await sp;
    const merged = mergeStates([acc, newState]);
    logObservedState("Update:", merged);
    runNext(n - 1, merged);
  }
}

const initial = observeInitialStates(queries);
logObservedState("Initial:", initial);
runNext(10, initial);

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
