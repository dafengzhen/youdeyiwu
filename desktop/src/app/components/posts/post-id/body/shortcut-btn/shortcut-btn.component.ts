import { Component, EventEmitter, Input, Output } from '@angular/core';
import { IPostDetails } from '@/src/types';

type TShortcutBtnComponentClickEventType = 'read' | 'cover';

export type IShortcutBtnComponentClickEvent = {
  type: TShortcutBtnComponentClickEventType;
  value: boolean;
};

@Component({
  selector: 'app-post-id-body-shortcut-btn',
  standalone: true,
  imports: [],
  templateUrl: './shortcut-btn.component.html',
  styleUrl: './shortcut-btn.component.scss',
})
export class ShortcutBtnComponent {
  readMore = false;
  expand = true;

  @Input() pending: boolean = false;
  @Input() details!: IPostDetails;

  @Output() clickEvent = new EventEmitter<IShortcutBtnComponentClickEvent>();

  onClickCollapseOrExpandCover() {
    const value = !this.expand;
    this.expand = value;
    this.clickEvent.emit({
      type: 'cover',
      value,
    });
  }

  onClickReadMore() {
    const value = !this.readMore;
    this.readMore = value;
    this.clickEvent.emit({
      type: 'read',
      value,
    });
  }
}
