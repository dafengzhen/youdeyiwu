import { createReducer, on } from '@ngrx/store';
import { login, logout } from '@/src/app/actions/global.actions';
import { IUser } from '@/src/types';

export interface IGlobalState {
  isLogin: boolean;
  loading: boolean;
  user?: IUser | null;
}

const initialState: IGlobalState = {
  isLogin: false,
  loading: true,
  user: null,
};

export const globalStateReducer = createReducer(
  initialState,
  on(login, (state, { user }): IGlobalState => {
    const _user = user ?? state.user;
    return {
      ...state,
      isLogin: !!_user,
      loading: false,
      user: _user,
    };
  }),
  on(
    logout,
    (state): IGlobalState => ({
      ...state,
      isLogin: false,
      loading: false,
      user: null,
    }),
  ),
);
