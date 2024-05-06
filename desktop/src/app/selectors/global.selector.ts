import { createSelector } from '@ngrx/store';
import { IGlobalState } from '@/src/app/reducers/global.reducer';
import { AppState } from '@/src/app/reducers';

const selectGlobal = (state: object) => (state as AppState).global;

export const selectGlobalSelector = createSelector(
  selectGlobal,
  (state: IGlobalState) => state,
);
