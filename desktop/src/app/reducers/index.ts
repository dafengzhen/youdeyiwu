import { isDevMode } from '@angular/core';
import { MetaReducer } from '@ngrx/store';
import {
  globalStateReducer,
  IGlobalState,
} from '@/src/app/reducers/global.reducer';

export interface AppState {
  global: IGlobalState;
}

export const reducers = {
  global: globalStateReducer,
};

export const metaReducers: MetaReducer<AppState>[] = isDevMode() ? [] : [];
