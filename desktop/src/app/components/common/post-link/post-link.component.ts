import { Component, Input } from '@angular/core';
import { IPost } from '@/src/types';

@Component({
  selector: 'app-post-link',
  standalone: true,
  imports: [],
  templateUrl: './post-link.component.html',
  styleUrl: './post-link.component.scss',
})
export class PostLinkComponent {
  @Input() post?: IPost;
}
