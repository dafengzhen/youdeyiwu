import { Component, Input } from '@angular/core';

@Component({
  selector: 'app-page-error',
  standalone: true,
  imports: [],
  templateUrl: './page-error.component.html',
  styleUrl: './page-error.component.scss',
})
export class PageErrorComponent {
  @Input({
    transform: (value: number) => {
      return value.toString();
    },
  })
  status = '400';
  @Input() message = '';
}
