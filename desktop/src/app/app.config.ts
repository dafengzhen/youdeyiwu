import { ApplicationConfig } from '@angular/core';
import { provideRouter, TitleStrategy } from '@angular/router';
import { provideHttpClient, withInterceptors } from '@angular/common/http';
import { provideAnimationsAsync } from '@angular/platform-browser/animations/async';
import { routes } from './app.routes';
import { authInterceptor } from './interceptors/auth.interceptor';
import { errorInterceptor } from './interceptors/error.interceptor';
import {
  provideAngularQuery,
  QueryClient,
} from '@tanstack/angular-query-experimental';
import { TemplatePageTitleStrategy } from '@/src/app/configs/template-page-title-strategy';
import { provideStore } from '@ngrx/store';
import { reducers } from '@/src/app/reducers';
import { provideEffects } from '@ngrx/effects';
import { effects } from '@/src/app/effects';

export const appConfig: ApplicationConfig = {
  providers: [
    provideAnimationsAsync(),
    provideRouter(routes),
    { provide: TitleStrategy, useClass: TemplatePageTitleStrategy },
    provideHttpClient(withInterceptors([authInterceptor, errorInterceptor])),
    provideAngularQuery(new QueryClient()),
    provideStore(reducers),
    provideEffects(effects),
  ],
};
