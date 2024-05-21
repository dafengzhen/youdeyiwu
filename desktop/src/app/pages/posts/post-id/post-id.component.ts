import { Component, OnDestroy, OnInit, signal } from '@angular/core';
import { NavComponent } from '@/src/app/components/posts/post-id/nav/nav.component';
import { BodyComponent } from '@/src/app/components/posts/post-id/body/body.component';
import { CoverComponent } from '@/src/app/components/posts/post-id/body/cover/cover.component';
import { HeadComponent } from '@/src/app/components/posts/post-id/head/head.component';
import { IShortcutBtnComponentClickEvent } from '@/src/app/components/posts/post-id/body/shortcut-btn/shortcut-btn.component';
import { injectQuery } from '@tanstack/angular-query-experimental';
import { IError, IPostDetails } from '@/src/types';
import { lastValueFrom, Subscription } from 'rxjs';
import { PostService } from '@/src/app/services/post.service';
import { ActivatedRoute, NavigationEnd, Router } from '@angular/router';
import { PageErrorComponent } from '@/src/app/components/common/page-error/page-error.component';
import { PageLoadingComponent } from '@/src/app/components/common/page-loading/page-loading.component';
import { parseURL } from '@/src/app/tools';

@Component({
  selector: 'app-post-id',
  standalone: true,
  imports: [
    NavComponent,
    BodyComponent,
    CoverComponent,
    HeadComponent,
    PageErrorComponent,
    PageLoadingComponent,
  ],
  templateUrl: './post-id.component.html',
  styleUrl: './post-id.component.scss',
})
export class PostIdComponent implements OnInit, OnDestroy {
  expand = true;
  previousPageId?: string | null;
  nextPageId?: string | null;
  routerEventsSubscribe?: Subscription;

  id = signal<string | null>(null);

  postDetailsQuery = injectQuery<IPostDetails, IError>(() => ({
    queryKey: [`/posts/details`, this.id()],
    queryFn: (context) => {
      const _id = context.queryKey[1] as string;
      return lastValueFrom(this.postService.queryDetails(_id));
    },
    enabled: typeof this.id() === 'string',
  }));

  constructor(
    private postService: PostService,
    private route: ActivatedRoute,
    private router: Router,
  ) {}

  onShortcutBtnClickEvent(e: IShortcutBtnComponentClickEvent) {
    if (e.type === 'cover') {
      this.expand = e.value;
    }
  }

  ngOnInit(): void {
    const id = this.route.snapshot.paramMap.get('id');
    const previousPageId =
      this.route.snapshot.queryParamMap.get('previousPageId');
    const nextPageId = this.route.snapshot.queryParamMap.get('nextPageId');
    this.id.set(id);
    this.previousPageId = previousPageId;
    this.nextPageId = nextPageId;

    this.routerEventsSubscribe = this.router.events.subscribe((event) => {
      if (event instanceof NavigationEnd) {
        const result = parseURL(event.url);
        const array = result.pathname.split('/');
        const value = array[array.length - 1];
        if (
          value === result.queryParams['nextPageId'] ||
          value === result.queryParams['previousPageId']
        ) {
          this.id.set(value);
        }
      }
    });
  }

  ngOnDestroy(): void {
    this.routerEventsSubscribe?.unsubscribe();
  }
}
