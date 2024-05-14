import { Component, Input } from '@angular/core';
import { IPostDetails } from '@/src/types';

@Component({
  selector: 'app-post-id-body-comments',
  standalone: true,
  imports: [],
  templateUrl: './comments.component.html',
  styleUrl: './comments.component.scss',
})
export class CommentsComponent {
  @Input() details!: IPostDetails;
}
