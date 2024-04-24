import { AfterViewInit, Component, ElementRef, ViewChild } from '@angular/core';
import { NavComponent } from '@/src/app/components/home/nav/nav.component';
import { FooterComponent } from '@/src/app/components/home/footer/footer.component';
import { PostsComponent } from '@/src/app/components/home/posts/posts.component';
import {
  IPageable,
  ISection,
  ISectionGroup,
  ITag,
  ITagGroup,
  TQueryParams,
} from '@/src/types';
import { TagsComponent } from '@/src/app/components/home/tags/tags.component';
import { TagsHeaderComponent } from '@/src/app/components/home/header/tags/header.component';
import { PostsHeaderComponent } from '@/src/app/components/home/header/posts/header.component';
import { TagGroupsComponent } from '@/src/app/components/home/tag-groups/tag-groups.component';
import { TagGroupsHeaderComponent } from '@/src/app/components/home/header/tag-groups/tag-groups.component';
import { SectionsComponent } from '@/src/app/components/home/sections/sections.component';
import { SectionsHeaderComponent } from '@/src/app/components/home/header/sections/sections.component';
import { SectionGroupsComponent } from '@/src/app/components/home/section-groups/section-groups.component';
import { SectionGroupsHeaderComponent } from '@/src/app/components/home/header/section-groups/section-groups.component';

type TNavName =
  | 'Content Groups'
  | 'Contents'
  | 'Tag Groups'
  | 'Tags'
  | 'Articles'
  | 'Create Article';

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
    TagGroupsComponent,
    TagGroupsComponent,
    TagGroupsComponent,
    TagGroupsHeaderComponent,
    SectionsComponent,
    SectionsHeaderComponent,
    SectionGroupsComponent,
    SectionGroupsHeaderComponent,
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
  showXBtn: boolean = false;
  selectedTagItem?: ITag;

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
    this.checkScrollbar();
  }

  onPage(v: number) {
    this.postsParams = { page: v };
  }

  onClickNav(name: TNavName) {
    if (name === 'Create Article') {
      return;
    }

    if (name === this.currentNavName) {
      this.onXBtn();
      return;
    }

    this.currentNavName = name;
    if (
      name === 'Tags' ||
      name === 'Tag Groups' ||
      name === 'Contents' ||
      name === 'Content Groups'
    ) {
      this.showPageable = false;
      this.showBackBtn = false;
    } else {
      this.showPageable = true;
      this.showBackBtn = false;
    }
  }

  onClickSectionGroups(item: ISectionGroup) {
    this.postsParams = { page: 0, sectionGroupId: item.id };
    this.lastNavName = this.currentNavName;
    this.currentNavName = 'Articles';
    this.showPageable = true;
    this.showBackBtn = true;
    this.showXBtn = true;
  }

  onClickSections(item: ISection) {
    this.postsParams = { page: 0, sectionId: item.id };
    this.lastNavName = this.currentNavName;
    this.currentNavName = 'Articles';
    this.showPageable = true;
    this.showBackBtn = true;
    this.showXBtn = true;
  }

  onClickTagGroups(item: ITagGroup) {
    this.postsParams = { page: 0, tagGroupId: item.id };
    this.lastNavName = this.currentNavName;
    this.currentNavName = 'Articles';
    this.showPageable = true;
    this.showBackBtn = true;
    this.showXBtn = true;
  }

  onClickTags(item: ITag) {
    this.postsParams = { page: 0, tagId: item.id };
    this.lastNavName = this.currentNavName;
    this.currentNavName = 'Articles';
    this.showPageable = true;
    this.showBackBtn = true;
    this.showXBtn = true;
  }

  onBackBtn() {
    this.postsParams = { page: 0 };
    this.currentNavName = this.lastNavName;
    this.showPageable = false;
    this.showBackBtn = false;
  }

  onXBtn() {
    if (this.selectedTagItem) {
      this.postsParams = { page: 0 };
      this.showPageable = true;
      this.showBackBtn = false;
      this.showXBtn = false;
      this.selectedTagItem = undefined;
      return;
    }

    this.postsParams = { page: 0 };
    this.currentNavName = 'Articles';
    this.showPageable = true;
    this.showBackBtn = false;
    this.showXBtn = false;
  }

  onTagItemEvent(item: ITag) {
    this.postsParams = { page: 0, tagId: item.id };
    this.selectedTagItem = item;
    this.showXBtn = true;
  }
}
