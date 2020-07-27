import { observeStates } from "../queries";

// @ts-ignore
const [queries] = arguments;

(function () {
  return observeStates(queries);
})();
