import { useSyncExternalStore } from 'react';

interface ISnapshotValue<T> {
  value: T | undefined;
}

const serverSnapshotCachedKey = { key: '' };
const serverSnapshotCachedValue: ISnapshotValue<unknown> = {
  value: null,
};

function useLocalStorage<T>(
  key: string,
  initialValue?: T,
): [ISnapshotValue<T>, (value: any) => void] {
  serverSnapshotCachedKey.key = key;
  serverSnapshotCachedValue.value = initialValue;
  const store = useSyncExternalStore(
    subscribe,
    getSnapshot,
    getServerSnapshot,
  ) as ISnapshotValue<T>;
  return [store, setValue];
}

function setValue(value: any) {
  localStorage.setItem(serverSnapshotCachedKey.key, JSON.stringify({ value }));
}

function getSnapshot() {
  const value = localStorage.getItem(serverSnapshotCachedKey.key);
  if (value) {
    serverSnapshotCachedValue.value = JSON.parse(value).value;
  }
  return serverSnapshotCachedValue;
}

function getServerSnapshot() {
  return serverSnapshotCachedValue;
}

function subscribe(callback: () => void) {
  window.addEventListener('storage', callback);
  return () => {
    window.removeEventListener('storage', callback);
  };
}

export default useLocalStorage;
