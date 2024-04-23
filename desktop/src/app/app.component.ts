import { Component } from '@angular/core';
import { RouterOutlet } from '@angular/router';
import { AngularQueryDevtools } from '@tanstack/angular-query-devtools-experimental';
import { environment } from '@/src/environments/environment';
import { animate, style, transition, trigger } from '@angular/animations';

export const fadeInOutAnimation = trigger('fadeInOut', [
  transition(':enter', [
    style({ opacity: 0 }),
    animate('1s ease-out', style({ opacity: 1 })),
  ]),
  transition(':leave', [animate('1s ease-in', style({ opacity: 0 }))]),
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
}
