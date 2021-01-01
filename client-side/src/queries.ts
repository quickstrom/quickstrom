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

export type Position = { x: number; y: number; width: number; height: number; }

export type ObservedState = Map<Selector, Array<{ element: Element; position: Position; elementState: Map<StateQuery, Value> }>>;

export type ObservedStateJSON = Array<[Selector, Array<{ element: Element; position: Position; elementState: Array<[StateQuery, Value]> }>]>;

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

  const elements = toArray(document.querySelectorAll(selector)) as Element[];
  const values = elements.map((element) => {
    var m = new Map();
    states.forEach((state) => {
      m.set(state, runStateQuery(element, state));
    });
    const rect = element.getBoundingClientRect();
    const position = {
      x: Math.round(rect.left),
      y: Math.round(rect.top),
      width: Math.round(rect.right - rect.left),
      height: Math.round(rect.bottom - rect.top),
    };
    return { position, element: element, elementState: m };
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

export function observeStateMap(queries: Queries): ObservedState {
  return mergeStates(queries.map(runQuery));
}

function observedStateToJSON(s: ObservedState): ObservedStateJSON {
  var r: ObservedStateJSON = [];
  s.forEach((v, k) => {
    r.push([k, v.map(e => ({ element: e.element, position: e.position, elementState: mapToArray(e.elementState) }))]);
  });
  return r;
}

export function observeState(queries: Queries): ObservedStateJSON {
  return observedStateToJSON(observeStateMap(queries));
}
