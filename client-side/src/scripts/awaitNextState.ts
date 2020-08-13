import { ObservedStateJSON } from "../queries";

const registeredObserver:
  | Promise<ObservedStateJSON | null>
  | undefined = (window as any).registeredObserver;

// @ts-ignore
const [done] = args;

(registeredObserver || Promise.resolve(null))
  .then((x) => ({
    Right: x,
  }))
  .catch((e) => ({
    Left: e,
  }))
  .then(done);
