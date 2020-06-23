import {
  NonEmptyArray,
  Optional,
  isNonEmpty,
  isNotNull,
  deepEqual,
  toArray,
  uuidv4,
} from "./essentials";

// QUERIES

type Selector = string;

type AttributeQuery = { tag: "attribute"; name: string };
type PropertyQuery = { tag: "property"; name: string };
type CssValueQuery = { tag: "cssValue"; name: string };
type TextQuery = { tag: "text" };

type StateQuery = AttributeQuery | PropertyQuery | CssValueQuery | TextQuery;

type Query = [Selector, StateQuery[]];

type Queries = Query[];

type Value = string | number | boolean | Element | null;

function runQuery([selector, states]: Query): ObservedState {
  function runStateQuery(element: Element, stateQuery: StateQuery): Value {
    switch (stateQuery.tag) {
      case "attribute":
        return element.attributes.getNamedItem(stateQuery.name) as
          | string
          | null;
      case "property":
        // console.log(stateQuery.name, element);
        // @ts-ignore
        return element[stateQuery.name];
      case "cssValue":
        return window
          .getComputedStyle(element)
          .getPropertyValue(stateQuery.name);
      case "text":
        return element.textContent;
    }
  }

  const values = toArray(document.querySelectorAll(selector)).map((element) => {
    var m = new Map();
    states.forEach((state) => {
      m.set(state, runStateQuery(element as Element, state));
    });
    return m;
  });

  return singletonMap(selector, values);
}

// ACTIONS

type ClickAction<T> = { tag: "click"; target: T };

type SelectedElement = { selector: Selector; index: number };

type Action = ClickAction<Selector>;

type SelectedAction = ClickAction<SelectedElement>;

function renderAction(action: SelectedAction): string {
  switch (action.tag) {
    case "click":
      return `click ${action.target.selector}[${action.target.index}]`;
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
  }
}

export function selectNextAction(
  actions: Array<Action>
): Optional<SelectedAction> {
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

export async function runAction(action: SelectedAction): Promise<void> {
  switch (action.tag) {
    case "click":
      const el = document.querySelectorAll(action.target.selector)[
        action.target.index
      ];
      (el as HTMLElement).click();
      return;
  }
}

// OBSERVATION

type ObservedState = Map<Selector, Array<Map<StateQuery, Value>>>;

function emptyMap<K, V>(): Map<K, V> {
  return new Map();
}

function singletonMap<K, V>(k: K, v: V): Map<K, V> {
  const m = new Map();
  m.set(k, v);
  return m;
}

function mergeStates(states: ObservedState[]): ObservedState {
  const s = new Map();
  states.forEach((state) => {
    state.forEach((values, key) => s.set(key, values));
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

function _observeInitialStates(queries: Queries): ObservedState {
  return mergeStates(queries.map(runQuery));
}

function matchesSelector(node: Node, selector: Selector): boolean {
  return node instanceof Element && node.matches(selector);
}

async function observeNextStateMutation(
  selector: Selector,
  stateQuery: StateQuery
): Promise<ObservedState> {
  return new Promise((resolve) => {
    new MutationObserver((mutations, observer) => {
      const anyMatching = mutations
        .flatMap((mutation) => {
          return [
            [mutation.target],
            toArray(mutation.addedNodes) as Node[],
            toArray(mutation.removedNodes) as Node[],
          ].flat();
        })
        .some((node) => matchesSelector(node, selector));

      observer.disconnect();
      const m: ObservedState = anyMatching
        ? runQuery([selector, [stateQuery]])
        : emptyMap();
      resolve(m);
    }).observe(document, { childList: true, subtree: true, attributes: true });
  });
}

async function observeNextPropertyChange(query: Query): Promise<ObservedState> {
  return new Promise(async (resolve) => {
    const initial = runQuery(query);
    for (let ms = 100; ; ms = ms * 2) {
      const current = runQuery(query);
      if (!isStateEqual(current, initial)) {
        resolve(current);
        return;
      }
      await delay(ms);
    }
  });
}

async function observeNextStateForStateQuery(
  selector: Selector,
  stateQuery: StateQuery
): Promise<ObservedState> {
  function observeNextEvent(
    eventType: string,
    extract: (node: any) => Value
  ): Promise<ObservedState> {
    return new Promise((resolve) => {
      (toArray(document.querySelectorAll(selector)) as Node[]).map(
        (element: Node) => {
          function handler(ev: Event) {
            element.removeEventListener(eventType, handler);
            if (ev.target != null) {
              const s: ObservedState = singletonMap(selector, [
                singletonMap(stateQuery, extract(ev.target as Node)),
              ]);
              resolve(s);
            } else {
              resolve(emptyMap());
            }
          }
          element.addEventListener(eventType, handler);
        }
      );
    });
  }
  switch (stateQuery.tag) {
    case "attribute":
      return observeNextStateMutation(selector, stateQuery);
    case "property":
      switch (stateQuery.name) {
        case "value":
          return Promise.race(
            ["keyup", "change"].map((eventType) =>
              observeNextEvent(
                eventType,
                (e: HTMLInputElement | HTMLTextAreaElement) => e.value
              )
            )
          );
        case "disabled":
          return observeNextPropertyChange([selector, [stateQuery]]);
        default:
          return observeNextStateMutation(selector, stateQuery);
      }
    case "cssValue":
      return observeNextStateMutation(selector, stateQuery);
    case "text":
      return observeNextStateMutation(selector, stateQuery);
  }
}

async function observeNextStateForQuery(query: Query): Promise<ObservedState> {
  await Promise.race(
    query[1].map((stateQuery) =>
      observeNextStateForStateQuery(query[0], stateQuery)
    )
  );
  return _observeInitialStates([query]);
}

function observeNextState(queries: Queries): Promise<ObservedState> {
  return Promise.race(queries.map(observeNextStateForQuery));
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

export async function runAndObserveAction(
  spec: Specification,
  currentState: ObservedState,
  action: SelectedAction
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

export async function runNext(
  spec: Specification,
  n: number,
  currentState: ObservedState
): Promise<void> {
  if (n <= (spec.maxActions || 100)) {
    const selected = selectNextAction(spec.actions);
    if (selected) {
      const newState = await runAndObserveAction(spec, currentState, selected);
      if (newState) {
        await runNext(spec, n + 1, newState);
      } else {
        console.warn("Stutter...");
        await runNext(spec, n + 1, currentState);
      }
    } else {
      console.warn("No more valid actions. Terminal state:", currentState);
    }
  } else {
    console.warn(
      "Ran maxmimum number of actions. Terminal state:",
      currentState
    );
  }
}

function _getNextState(id: string): Promise<ObservedState> {
  return (
    registeredObservers.get(id) ||
    Promise.reject(`No registered state observer for ID: ${id}`)
  );
}

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

const registeredObservers: Map<string, Promise<ObservedState>> = new Map();

export function registerNextStateObserver(queries: Queries): string {
  const id = uuidv4();
  const p = Promise.race([
    observeNextState(queries),
    delay(1000).then(() => _observeInitialStates(queries)),
  ]);
  registeredObservers.set(id, p);
  return id;
}

type ObservedStateJSON = Array<[Selector, Array<Array<[StateQuery, Value]>>]>;

function observedStateToJSON(s: ObservedState): ObservedStateJSON {
  var r: ObservedStateJSON = [];
  s.forEach((v, k) => {
    // throw Error(v.toString());
    r.push([k, v.map(mapToArray)]);
  });
  return r;
}

export function getNextState(id: string): Promise<ObservedStateJSON> {
  return _getNextState(id).then(observedStateToJSON);
}

export function observeInitialStates(queries: Queries): ObservedStateJSON {
  return observedStateToJSON(_observeInitialStates(queries));
}

type Either<a, b> = { Left: a } | { Right: b };

export function runPromiseEither<A>(
  promise: Promise<A>,
  done: (either: Either<Error, A>) => void
): void {
  promise
    .then((a) => done({ Right: a }))
    .catch((err) => done({ Left: err.message }));
}

export function mapToArray<K, V>(m: Map<K, V>): [K, V][] {
  return Array.from(m.entries());
}

export function mapNullable<A, B>(
  f: (a: A) => B
): (oa: Optional<A>) => Optional<B> {
  return (a) => (a ? f(a) : null);
}
