import { observeState } from "../queries";

// @ts-ignore
const [queries, done] = args;

try {
  done({ Right: observeState(queries) });
} catch (e) {
  done({ Left: e });
}
