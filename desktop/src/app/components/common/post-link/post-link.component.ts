import { Component, EventEmitter, Input, Output } from '@angular/core';
import { IPost, ITag } from '@/src/types';
import { NgIf } from '@angular/common';
import { IsTodayPipe } from '@/src/app/pipes/is-today.pipe';

@Component({
  selector: 'app-post-link',
  standalone: true,
  imports: [NgIf, IsTodayPipe],
  templateUrl: './post-link.component.html',
  styleUrl: './post-link.component.scss',
})
export class PostLinkComponent {
  @Input() post?: IPost;

  @Output() tagItemEvent = new EventEmitter<ITag>();

  onClickTag(e: MouseEvent, item: ITag) {
    e.stopPropagation();
    e.preventDefault();

    this.tagItemEvent.emit(item);
  }
}
