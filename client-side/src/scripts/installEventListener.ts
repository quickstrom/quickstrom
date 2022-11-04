import { awaitChanged, awaitLoaded, awaitStyleChanged } from "../events";

window.quickstrom.run = function(queries) {
    const selectors = Object.keys(queries);
    window.quickstrom.eventsObserver = Promise.race([
        awaitLoaded(),
        awaitChanged(selectors),
        awaitStyleChanged(queries),
    ]);
};
