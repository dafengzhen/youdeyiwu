import { AfterViewInit, Component, ElementRef, ViewChild } from '@angular/core';
import { NavComponent } from '@/src/app/components/home/nav/nav.component';
import { FooterComponent } from '@/src/app/components/home/footer/footer.component';
import { PostsComponent } from '@/src/app/components/home/posts/posts.component';
import { IPageable, ITag, TQueryParams } from '@/src/types';
import { TagsComponent } from '@/src/app/components/home/tags/tags.component';
import { TagsHeaderComponent } from '@/src/app/components/home/header/tags/header.component';
import { PostsHeaderComponent } from '@/src/app/components/home/header/posts/header.component';

type TNavName =
  | 'Content Groups'
  | 'Contents'
  | 'Tag Groups'
  | 'Tags'
  | 'Articles';

@Component({
  selector: 'app-home',
  standalone: true,
  imports: [
    NavComponent,
    FooterComponent,
    PostsComponent,
    TagsComponent,
    TagsHeaderComponent,
    PostsHeaderComponent,
  ],
  templateUrl: './home.component.html',
  styleUrl: './home.component.scss',
})
export class HomeComponent implements AfterViewInit {
  @ViewChild('myDiv') myDiv: ElementRef | undefined;

  postsParams: TQueryParams = { page: 0 };
  pageable?: IPageable;
  currentNavName?: TNavName;
  lastNavName?: TNavName;
  showPageable: boolean = true;
  showBackBtn: boolean = false;

  ngAfterViewInit(): void {
    this.checkScrollbar();
  }

  checkScrollbar(): void {
    if (!this.myDiv) {
      return;
    }

    const hasScrollbar =
      this.myDiv.nativeElement.scrollHeight >
      this.myDiv.nativeElement.clientHeight;
    if (hasScrollbar) {
      this.myDiv.nativeElement.classList.add('yw-home-margin-right');
    } else {
      this.myDiv.nativeElement.classList.remove('yw-home-margin-right');
    }
  }

  onPageablePosts(value: IPageable) {
    this.pageable = value;
  }

  onPage(v: number) {
    this.postsParams = { page: v };
  }

  onClickNav(name: TNavName) {
    if (name === this.currentNavName) {
      return;
    }

    this.currentNavName = name;
    if (name === 'Tags') {
      this.showPageable = false;
      this.showBackBtn = false;
    } else {
      this.showPageable = true;
      this.showBackBtn = false;
    }
  }

  onClickTags(item: ITag) {
    this.postsParams = { page: 0, tagId: item.id };
    this.lastNavName = this.currentNavName;
    this.currentNavName = 'Articles';
    this.showPageable = true;
    this.showBackBtn = true;
  }

  onBackBtn() {
    this.postsParams = { page: 0 };
    this.currentNavName = this.lastNavName;
    this.showPageable = false;
    this.showBackBtn = false;
  }
}
