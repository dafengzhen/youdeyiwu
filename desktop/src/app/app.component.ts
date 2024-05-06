import { Component } from '@angular/core';
import { RouterOutlet } from '@angular/router';
import { AngularQueryDevtools } from '@tanstack/angular-query-devtools-experimental';
import { environment } from '@/src/environments/environment';
import {
  animate,
  keyframes,
  style,
  transition,
  trigger,
} from '@angular/animations';
import { Store } from '@ngrx/store';
import { loadUserLoginInfo } from '@/src/app/actions/global.actions';

export const fadeInOutAnimation = trigger('fadeInOut', [
  transition(':enter', [
    style({
      opacity: 0,
      transform: 'scale(0)',
    }),
    animate(
      '1.5s cubic-bezier(0.4, 0, 0.2, 1)',
      keyframes([
        style({
          opacity: 0,
          transform: 'scale(0)',
          offset: 0,
        }),
        style({
          opacity: 0.3,
          transform: 'scale(0.2)',
          offset: 0.2,
        }),
        style({
          opacity: 0.6,
          transform: 'scale(0.4)',
          offset: 0.4,
        }),
        style({
          opacity: 1,
          transform: 'scale(0.7)',
          offset: 0.7,
        }),
        style({
          opacity: 1,
          transform: 'scale(1)',
          offset: 1,
        }),
      ]),
    ),
  ]),
  transition(':leave', [
    animate(
      '1.5s cubic-bezier(0.4, 0, 0.2, 1)',
      keyframes([
        style({
          opacity: 1,
          transform: 'scale(1)',
          offset: 0,
        }),
        style({
          opacity: 0.7,
          transform: 'scale(0.8)',
          offset: 0.3,
        }),
        style({
          opacity: 0.3,
          transform: 'scale(0.6)',
          offset: 0.6,
        }),
        style({
          opacity: 0,
          transform: 'scale(0)',
          offset: 1,
        }),
      ]),
    ),
  ]),
]);

@Component({
  selector: 'app-root',
  standalone: true,
  imports: [RouterOutlet, AngularQueryDevtools],
  templateUrl: './app.component.html',
  styleUrl: './app.component.scss',
  animations: [fadeInOutAnimation],
})
export class AppComponent {
  title = 'youdeyiwu';

  protected readonly environment = environment;

  constructor(private store: Store) {
    this.loadUserInfo();
  }

  loadUserInfo() {
    this.store.dispatch(loadUserLoginInfo());
  }
}
