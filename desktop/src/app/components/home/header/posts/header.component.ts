import { Component, Input } from '@angular/core';

@Component({
  selector: 'app-home-posts-header',
  standalone: true,
  imports: [],
  templateUrl: './header.component.html',
  styleUrl: './header.component.scss',
})
export class PostsHeaderComponent {
  @Input() hideOverview: boolean = true;
}
