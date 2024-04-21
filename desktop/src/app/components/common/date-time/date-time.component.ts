import { Component, Input } from '@angular/core';
import { FromNowPipe } from '@/src/app/pipes/from-now.pipe';

@Component({
  selector: 'app-date-time',
  standalone: true,
  imports: [FromNowPipe],
  templateUrl: './date-time.component.html',
  styleUrl: './date-time.component.scss',
})
export class DateTimeComponent {
  @Input() time!: string;
}
