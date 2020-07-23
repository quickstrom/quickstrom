namespace WebCheck {
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

    const values = toArray(document.querySelectorAll(selector)).map(
      (element) => {
        var m = new Map();
        states.forEach((state) => {
          m.set(state, runStateQuery(element as Element, state));
        });
        return m;
      }
    );

    return singletonMap(selector, values);
  }

  // OBSERVATION

  type ObservedState = Map<Selector, Array<Map<StateQuery, Value>>>;

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

  function _observeStates(queries: Queries): ObservedState {
    return mergeStates(queries.map(runQuery));
  }

  type ObservedStateJSON = Array<[Selector, Array<Array<[StateQuery, Value]>>]>;

  function observedStateToJSON(s: ObservedState): ObservedStateJSON {
    var r: ObservedStateJSON = [];
    s.forEach((v, k) => {
      r.push([k, v.map(mapToArray)]);
    });
    return r;
  }

  function mapToArray<K, V>(m: Map<K, V>): [K, V][] {
    return Array.from(m.entries());
  }

  function toArray<T, A extends { [index: number]: T }>(xs: A): T[] {
    return Array.prototype.slice.call(xs);
  }

  export function observeStates(queries: Queries): ObservedStateJSON {
    return observedStateToJSON(_observeStates(queries));
  }
}

// @ts-ignore
return WebCheck.observeStates(arguments[0]);
