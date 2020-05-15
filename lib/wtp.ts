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

function runQuery(query: Query): Array<any> {
    function runStateQuery(
        element: Element,
        stateQuery: StateQuery
    ): any {
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
                    !element.disabled
                );
        }
    }
    switch (query.tag) {
        case "element":
            return Array.prototype.slice.call(document.querySelectorAll(query.selector));
        case "elementState":
            return runQuery(query.elementQuery).map((element) =>
                runStateQuery(element, query.stateQuery)
            );
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

// OBSERVATION

export function observeQuery<Q extends Query>(query: Q) {
    const selector = selectorInQuery(query);

    const result = runQuery(query);
    console.log("Initial state:", result);

    function recordMatching(node: Node) {
        if (node instanceof Element && node.matches(selector)) {
            const result = runQuery(query);
            console.log("Queried element changed. New query results:", result);
        }
    }

    new MutationObserver((mutations, _observer) => {
        mutations.forEach((mutation) => {
            recordMatching(mutation.target as Element);
            mutation.addedNodes.forEach((node, _key, _parent) =>
                recordMatching(node as Element)
            );
            mutation.removedNodes.forEach((node, _key, _parent) =>
                recordMatching(node as Element)
            );
        });
    }).observe(document, { childList: true, subtree: true, attributes: true });
}

// DEMO

const q1 = enabled(byCss("button"));
const r1: Array<string> = runQuery(q1);
observeQuery(q1);
observeQuery(text(byCss(".message")));

// OLD STUFF

export function awaitElement(sel: string, done: () => void) {
    var timer = setInterval(function() {
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
