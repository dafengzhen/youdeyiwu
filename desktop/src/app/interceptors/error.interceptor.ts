import { HttpHandlerFn, HttpRequest } from '@angular/common/http';
import { catchError, throwError } from 'rxjs';
import { inject } from '@angular/core';
import { CookieService } from 'ngx-cookie-service';
import { TK } from '@/src/app/constants';

export function errorInterceptor(
  req: HttpRequest<unknown>,
  next: HttpHandlerFn,
) {
  const cookieService = inject(CookieService);
  return next(req).pipe(
    catchError((err) => {
      if (err.status === 401) {
        cookieService.delete(TK);
      }

      return throwError(() => err);
    }),
  );
}
