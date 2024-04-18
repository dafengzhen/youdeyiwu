import { Component } from '@angular/core';
import { RouterOutlet } from '@angular/router';
import { AngularQueryDevtools } from '@tanstack/angular-query-devtools-experimental';

@Component({
  selector: 'app-root',
  standalone: true,
  imports: [RouterOutlet, AngularQueryDevtools],
  templateUrl: './app.component.html',
  styleUrl: './app.component.scss',
})
export class AppComponent {
  title = 'youdeyiwu';
}
