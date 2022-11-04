export function mapToArray<K, V>(m: Map<K, V>): [K, V][] {
  return Array.from(m.entries());
}

export function toArray<T, A extends { [index: number]: T }>(xs: A): T[] {
  return Array.prototype.slice.call(xs);
}

export function distinct<T>(i: T[]): T[] {
  const r = new Set<T>();
  i.forEach((n) => r.add(n));
  return Array.from(r);
}
