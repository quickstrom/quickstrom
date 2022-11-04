import { queryState } from "../queries";

window.quickstrom.run = function (queries) {
    return queryState(queries)
};
