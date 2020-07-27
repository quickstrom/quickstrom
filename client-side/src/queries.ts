import { mapToArray, toArray } from "./arrays";

export type Selector = string;

export type AttributeQuery = { tag: "attribute"; name: string };
export type PropertyQuery = { tag: "property"; name: string };
export type CssValueQuery = { tag: "cssValue"; name: string };
export type TextQuery = { tag: "text" };

export type StateQuery =
  | AttributeQuery
  | PropertyQuery
  | CssValueQuery
  | TextQuery;

export type Query = [Selector, StateQuery[]];

export type Queries = Query[];

export type Value = string | number | boolean | Element | null;

export type ObservedState = Map<Selector, Array<Map<StateQuery, Value>>>;

export type ObservedStateJSON = Array<[Selector, Array<Array<[StateQuery, Value]>>]>;

export function runQuery([selector, states]: Query): ObservedState {
  function runStateQuery(element: Element, stateQuery: StateQuery): Value {
    switch (stateQuery.tag) {
      case "attribute":
        return element.attributes.getNamedItem(stateQuery.name)?.value as
          | string
          | null;
      case "property":
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

export function observeStatesMap(queries: Queries): ObservedState {
  return mergeStates(queries.map(runQuery));
}

function observedStateToJSON(s: ObservedState): ObservedStateJSON {
  var r: ObservedStateJSON = [];
  s.forEach((v, k) => {
    r.push([k, v.map(mapToArray)]);
  });
  return r;
}

export function observeStates(queries: Queries): ObservedStateJSON {
  return observedStateToJSON(observeStatesMap(queries));
}
