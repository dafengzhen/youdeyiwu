import { HttpHandlerFn, HttpRequest } from '@angular/common/http';
import { catchError, throwError } from 'rxjs';

export function errorInterceptor(
  req: HttpRequest<unknown>,
  next: HttpHandlerFn,
) {
  return next(req).pipe(catchError((err) => throwError(() => err)));
}
