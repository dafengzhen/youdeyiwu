import { Component, Input } from '@angular/core';
import { FromNowPipe } from '@/src/app/pipes/from-now.pipe';
import {IsTodayPipe} from "@/src/app/pipes/is-today.pipe";

@Component({
  selector: 'app-date-time',
  standalone: true,
  imports: [FromNowPipe, IsTodayPipe],
  templateUrl: './date-time.component.html',
  styleUrl: './date-time.component.scss',
})
export class DateTimeComponent {
  @Input() time!: string;
}
