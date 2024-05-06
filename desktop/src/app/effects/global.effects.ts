import { Injectable } from '@angular/core';
import { Actions, createEffect, ofType } from '@ngrx/effects';
import { catchError, exhaustMap, map } from 'rxjs/operators';
import { of } from 'rxjs';
import {
  loadUserLoginInfo,
  login,
  logout,
} from '@/src/app/actions/global.actions';
import { UserService } from '@/src/app/services/user.service';

@Injectable()
export class GlobalEffects {
  constructor(
    private actions$: Actions,
    private userService: UserService,
  ) {}

  loadUserLoginInfo$ = createEffect(() => {
    return this.actions$.pipe(
      ofType(loadUserLoginInfo),
      exhaustMap(() =>
        this.userService.loginInfo().pipe(
          map((user) => login({ user })),
          catchError(() => of(logout())),
        ),
      ),
    );
  });
}
