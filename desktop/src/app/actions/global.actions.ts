import { createAction, props } from '@ngrx/store';
import { IUser } from '@/src/types';

export const login = createAction(
  '[Global] Login',
  props<{
    user?: IUser | null;
  }>(),
);

export const logout = createAction('[Global] Logout');

export const loadUserLoginInfo = createAction('[Global] Load User Login Info');
