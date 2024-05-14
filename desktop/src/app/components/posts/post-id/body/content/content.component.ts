import { Component, Input } from '@angular/core';
import { IPostDetails } from '@/src/types';

@Component({
  selector: 'app-post-id-body-content',
  standalone: true,
  imports: [],
  templateUrl: './content.component.html',
  styleUrl: './content.component.scss',
})
export class ContentComponent {
  @Input() details!: IPostDetails;
}
