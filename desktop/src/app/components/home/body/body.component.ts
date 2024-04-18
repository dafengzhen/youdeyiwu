import { Component } from '@angular/core';
import { NgOptimizedImage } from '@angular/common';
import { injectQuery } from '@tanstack/angular-query-experimental';
import { lastValueFrom } from 'rxjs';
import { PostService } from '@/src/app/services/post.service';

@Component({
  selector: 'app-home-body',
  standalone: true,
  imports: [NgOptimizedImage],
  templateUrl: './body.component.html',
  styleUrl: './body.component.scss',
})
export class BodyComponent {
  postsQuery = injectQuery(() => ({
    queryKey: ['/posts/select-all'],
    queryFn: () => lastValueFrom(this.postService.selectAll()),
  }));

  constructor(private postService: PostService) {}
}
