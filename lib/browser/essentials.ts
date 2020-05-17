// https://stackoverflow.com/a/25456134
export function deepEqual(x: any, y: any) {
  if (x === y) {
    return true;
  } else if (
    typeof x == "object" &&
    x != null &&
    typeof y == "object" &&
    y != null
  ) {
    if (Object.keys(x).length != Object.keys(y).length) return false;

    for (var prop in x) {
      if (y.hasOwnProperty(prop)) {
        if (!deepEqual(x[prop], y[prop])) return false;
      } else return false;
    }

    return true;
  } else return false;
}

export function toArray<T, A extends { [index: number]: T }>(xs: A): T[] {
  return Array.prototype.slice.call(xs);
}

export type Optional<T> = T | null;

export function isNotNull<T>(x: Optional<T>): x is T {
  return x !== null;
}

export type NonEmptyArray<T> = [T, ...T[]];

export function isNonEmpty<T>(arr: T[]): arr is NonEmptyArray<T> {
  return arr.length > 0;
}
