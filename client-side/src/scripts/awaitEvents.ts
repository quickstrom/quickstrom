import { toDetached } from "../events";
import { queryState, Dependencies } from "../queries";

function delay(ms: number): Promise<null> {
    return new Promise((resolve) => {
        setTimeout(resolve, ms);
    });
}

window.quickstrom.run = function(queries: Dependencies, timeoutMs: number, done: any) {
    Promise.race([
        window.quickstrom.eventsObserver,
        delay(timeoutMs),
    ]).then((events) => {
        if (events) {
            done({ events: events.map(toDetached), state: queryState(queries) });
        } else {
            done(null);
        }
    });
};
