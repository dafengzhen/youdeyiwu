import { Component, EventEmitter, Input, Output } from '@angular/core';
import { NameComponent } from '@/src/app/components/posts/post-id/body/name/name.component';
import { OverviewComponent } from '@/src/app/components/posts/post-id/body/overview/overview.component';
import {
  IShortcutBtnComponentClickEvent,
  ShortcutBtnComponent,
} from '@/src/app/components/posts/post-id/body/shortcut-btn/shortcut-btn.component';
import { ContentComponent } from '@/src/app/components/posts/post-id/body/content/content.component';
import { CommentsComponent } from '@/src/app/components/posts/post-id/body/comments/comments.component';
import { IPostDetails } from '@/src/types';

@Component({
  selector: 'app-post-id-body',
  standalone: true,
  imports: [
    NameComponent,
    OverviewComponent,
    ShortcutBtnComponent,
    ContentComponent,
    CommentsComponent,
  ],
  templateUrl: './body.component.html',
  styleUrl: './body.component.scss',
})
export class BodyComponent {
  readMore: boolean = false;

  @Input() pending: boolean = false;
  @Input() details!: IPostDetails;

  @Output() shortcutBtnClickEvent =
    new EventEmitter<IShortcutBtnComponentClickEvent>();

  onShortcutBtnClickEvent(e: IShortcutBtnComponentClickEvent) {
    if (e.type === 'read') {
      this.readMore = e.value;
    }

    this.shortcutBtnClickEvent.emit(e);
  }
}
