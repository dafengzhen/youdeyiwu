import { Component, Input } from '@angular/core';
import { IPostDetails } from '@/src/types';

@Component({
  selector: 'app-post-id-body-overview',
  standalone: true,
  imports: [],
  templateUrl: './overview.component.html',
  styleUrl: './overview.component.scss',
})
export class OverviewComponent {
  @Input() details!: IPostDetails;
}
