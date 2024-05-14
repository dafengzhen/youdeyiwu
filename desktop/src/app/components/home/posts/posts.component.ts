import { Component, EventEmitter, Input, input, Output } from '@angular/core';
import { AsyncPipe, NgOptimizedImage } from '@angular/common';
import { injectQuery } from '@tanstack/angular-query-experimental';
import { lastValueFrom } from 'rxjs';
import { PostService } from '@/src/app/services/post.service';
import { UserAliasPipe } from '@/src/app/pipes/user-alias.pipe';
import { UserAvatarComponent } from '@/src/app/components/common/user-avatar/user-avatar.component';
import { FromNowPipe } from '@/src/app/pipes/from-now.pipe';
import { DateTimeComponent } from '@/src/app/components/common/date-time/date-time.component';
import { UserLinkComponent } from '@/src/app/components/common/user-link/user-link.component';
import { PostLinkComponent } from '@/src/app/components/common/post-link/post-link.component';
import { IError, IPageable, IPost, ITag, TQueryParams } from '@/src/types';
import { createSequenceArray } from '@/src/app/tools';
import { PageErrorComponent } from '@/src/app/components/common/page-error/page-error.component';
import { PageLoadingComponent } from '@/src/app/components/common/page-loading/page-loading.component';

@Component({
  selector: 'app-home-posts',
  standalone: true,
  imports: [
    NgOptimizedImage,
    UserAliasPipe,
    UserAvatarComponent,
    FromNowPipe,
    DateTimeComponent,
    UserLinkComponent,
    PostLinkComponent,
    AsyncPipe,
    PageErrorComponent,
    PageLoadingComponent,
  ],
  templateUrl: './posts.component.html',
  styleUrl: './posts.component.scss',
})
export class PostsComponent {
  params = input<TQueryParams>({ page: 0 });
  placeholders: number[] = createSequenceArray(1);

  @Input() hideOverview: boolean = true;

  @Output() pageableEvent = new EventEmitter<IPageable>();
  @Output() tagItemEvent = new EventEmitter<ITag>();

  postsQuery = injectQuery<IPost[], IError>(() => ({
    queryKey: ['/posts/select-all', this.params()],
    queryFn: async (context) => {
      const _params = context.queryKey[1] as TQueryParams;
      const response = await lastValueFrom(this.postService.selectAll(_params));
      const pageSize = response.content.length;
      this.placeholders = createSequenceArray(pageSize);
      this.pageableEvent.emit({
        ...response.pageable,
        currentPageSize: pageSize,
      });
      return response.content.map((item, index, array) => {
        const previousPageIndex = Math.max(0, index - 1);
        const nextPageIndex = Math.min(array.length, index + 1);
        if (previousPageIndex !== index) {
          item.previousPageId = array[previousPageIndex]?.id;
        }

        if (nextPageIndex !== index) {
          item.nextPageId = array[nextPageIndex]?.id;
        }
        return item;
      });
    },
  }));

  constructor(private postService: PostService) {}

  onTagItemEvent(item: ITag) {
    this.tagItemEvent.emit(item);
  }
}
