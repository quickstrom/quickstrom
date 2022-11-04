import { distinct, toArray } from "./arrays";
import { Dependencies } from "./queries";

type LoadedEvent = { tag: "loaded" };

type ChangedEvent = { tag: "changed"; element: Element };

type DetachedEvent = { tag: "detached"; markup: string };

export function toDetached(event: LoadedEvent | ChangedEvent): LoadedEvent | ChangedEvent | DetachedEvent {
  switch (event.tag) {
    case "loaded":
      return event;
    case "changed":
      if (event.element.isConnected) {
        return event;
      } else {
        return { tag: "detached", markup: renderMarkupSummary(event.element) };
      }
  }
}

function matchesSelector(node: Node, selector: string): boolean {
  return node instanceof Element && node.matches(selector);
}

function matchesAnySelector(node: Node, selectors: string[]): boolean {
  return (
    selectors.find((selector) => matchesSelector(node, selector)) !== undefined
  );
}

function matchingAnySelector(nodes: Node[], selectors: string[]): Element[] {
  const elements = nodes.filter((node) =>
    matchesAnySelector(node, selectors)
  ) as Element[];
  return distinct(elements);
}

export async function awaitLoaded(): Promise<LoadedEvent[]> {
  return new Promise((resolve) => {
    // @ts-ignore
    const alreadyFired = window.quickstromLoadedFired;
    if (document.readyState === "complete" && !alreadyFired) {
      // @ts-ignore
      window.quickstromLoadedFired = true;
      resolve([{ tag: "loaded" }]);
    } else {
      window.addEventListener("load", () => {
        resolve([{ tag: "loaded" }]);
      });
    }
  });
}

export async function awaitStyleChanged(
  deps: Dependencies
): Promise<ChangedEvent[]> {
  const events: Array<keyof DocumentEventMap> = [
    "transitionend",
    "transitioncancel",
    "animationend",
    "animationcancel",
  ];
  return new Promise((resolve) => {
    function remove() {
      events.forEach((name) =>
        document.removeEventListener(name, onEnd as EventListener)
      );
    }
    function onEnd(e: AnimationEvent | TransitionEvent) {
      if (e.target && e.target instanceof Element) {
        const anyMatching =
          Object.entries(deps).find(([selector, schema]) => {
            if (e instanceof TransitionEvent) {
              return (
                schema.css &&
                schema.css[e.propertyName] &&
                matchesSelector(e.target as Element, selector)
              );
            } else {
              return matchesSelector(e.target as Element, selector);
            }
          }) !== undefined;
        if (anyMatching) {
          remove();
          resolve([{ tag: "changed", element: e.target }]);
        }
      }
    }
    events.forEach((name) =>
      document.addEventListener(name, onEnd as EventListener)
    );
  });
}

export async function awaitChanged(
  selectors: string[]
): Promise<ChangedEvent[]> {
  return new Promise((resolve) => {
    new MutationObserver((mutations, observer) => {
      const nodes = mutations.flatMap((mutation) => {
        return [
          [mutation.target],
          toArray(mutation.addedNodes) as Node[],
          toArray(mutation.removedNodes) as Node[],
        ].flat();
      });

      const matching = matchingAnySelector(nodes, selectors);
      if (matching.length > 0) {
        observer.disconnect();
        resolve(matching.map((e) => ({ tag: "changed", element: e })));
      }
    }).observe(document, {
      childList: true,
      subtree: true,
      attributes: true,
    });
  });
}

function renderMarkupSummary(element: Element) {
  const tag = element.tagName;
  let attrs: string[] = [];
  element.getAttributeNames().forEach((name) => {
    const value = element.getAttribute(name);
    if (value !== null) {
      attrs.push(`${name}="${value.replace('"', '\\"')}"`);
    } else {
      attrs.push(`${name}"`);
    }
  });
  return `<${tag}${attrs.join("")}>...</${tag}>`;
}
