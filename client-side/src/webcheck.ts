
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


function delay(ms: number): Promise<void> {
  return new Promise((resolve) => {
    setTimeout(resolve, ms);
  });
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
): Promise<void> {
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
      resolve();
    }).observe(document, { childList: true, subtree: true, attributes: true });
  });
}

async function observeNextPropertyChange(query: Query): Promise<void> {
  return new Promise(async (resolve) => {
    const initial = runQuery(query);
    for (let ms = 100; ; ms = ms * 2) {
      const current = runQuery(query);
      if (!isStateEqual(current, initial)) {
        return resolve();
      }
      await delay(ms);
    }
  });
}

async function observeNextStateForStateQuery(
  selector: Selector,
  stateQuery: StateQuery
): Promise<void> {
  function observeNextEvent(
    eventType: string,
    extract: (node: any) => Value
  ): Promise<void> {
    return new Promise((resolve) => {
      (toArray(document.querySelectorAll(selector)) as Node[]).map(
        (element: Node) => {
          function handler(ev: Event) {
            element.removeEventListener(eventType, handler);
            if (ev.target != null) {
              const s: ObservedState = singletonMap(selector, [
                singletonMap(stateQuery, extract(ev.target as Node)),
              ]);
              resolve();
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
      return observeNextPropertyChange([selector, [stateQuery]]);
    case "text":
      return observeNextStateMutation(selector, stateQuery);
  }
}

async function observeNextStateForQuery(query: Query): Promise<void> {
  return await Promise.race(
    query[1].map((stateQuery) =>
      observeNextStateForStateQuery(query[0], stateQuery)
    )
  );
}

async function observeNextState(queries: Queries): Promise<ObservedState> {
  await Promise.race(queries.map(observeNextStateForQuery));
  return _observeInitialStates(queries);
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
    delay(100).then(() => _observeInitialStates(queries)),
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

/**
 * ESSENTIALS
 */

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

export function uuidv4(): string {
    return 'xxxxxxxx-xxxx-4xxx-yxxx-xxxxxxxxxxxx'.replace(/[xy]/g, function(c) {
        const r = Math.random() * 16 | 0, v = c == 'x' ? r : (r & 0x3 | 0x8);
        return v.toString(16);
    });
}