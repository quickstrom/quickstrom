const registeredObserver:
  | Promise<void | null>
  | undefined = (window as any).registeredObserver;

// @ts-ignore
const [done] = args;

(registeredObserver || Promise.resolve(null))
  .then(_ => ({
    Right: [],
  }))
  .catch((e) => ({
    Left: e,
  }))
  .then(done);
