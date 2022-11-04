export function emptyMap<K, V>(): Map<K, V> {
  return new Map();
}

export function singletonMap<K, V>(k: K, v: V): Map<K, V> {
  const m = new Map();
  m.set(k, v);
  return m;
}