import { HttpHandlerFn, HttpRequest } from '@angular/common/http';
import { inject } from '@angular/core';
import { CookieService } from 'ngx-cookie-service';
import { AUTHORIZATION, BEARER, TK } from '../constants';
import { environment } from '../../environments/environment';

export function authInterceptor(
  req: HttpRequest<unknown>,
  next: HttpHandlerFn,
) {
  const apiUrl = environment.apiUrl;
  const cookieService = inject(CookieService);

  const headers: { [AUTHORIZATION]?: string } = {};
  if (cookieService.check(TK)) {
    headers[AUTHORIZATION] = `${BEARER} ${cookieService.get(TK)}`;
  }

  const reqWithBaseUrl = req.clone({
    url: apiUrl + req.url,
    setHeaders: headers,
  });

  return next(reqWithBaseUrl);
}
