import { Injectable } from '@angular/core';

@Injectable({
  providedIn: 'root',
})
export class LocalStorageService {
  private prefix = '_ng_';

  constructor() {}

  setItem(key: string, value: unknown) {
    const _key = this.prefix + key;
    localStorage.setItem(
      _key,
      JSON.stringify({
        value,
      }),
    );
  }

  getItem(key: string) {
    const _key = this.prefix + key;
    const item = localStorage.getItem(_key);
    if (item) {
      return JSON.parse(item).value;
    }
  }

  removeItem(key: string) {
    const _key = this.prefix + key;
    localStorage.removeItem(_key);
  }

  clear() {
    localStorage.clear();
  }
}
