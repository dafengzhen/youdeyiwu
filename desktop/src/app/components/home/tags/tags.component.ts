import { Component, EventEmitter, Output } from '@angular/core';
import { IError, ITag } from '@/src/types';
import { injectQuery } from '@tanstack/angular-query-experimental';
import { lastValueFrom } from 'rxjs';
import { TagService } from '@/src/app/services/tag.service';
import { FromNowPipe } from '@/src/app/pipes/from-now.pipe';

@Component({
  selector: 'app-home-tags',
  standalone: true,
  imports: [FromNowPipe],
  templateUrl: './tags.component.html',
  styleUrl: './tags.component.scss',
})
export class TagsComponent {
  @Output() itemEvent = new EventEmitter<ITag>();

  tagsQuery = injectQuery<ITag[], IError>(() => ({
    queryKey: ['/tags/select-all'],
    queryFn: async () => {
      return await lastValueFrom(this.tagService.selectAll());
    },
  }));

  constructor(private tagService: TagService) {}

  onClickItem(item: ITag) {
    this.itemEvent.emit(item);
  }

  onKeyup(e: KeyboardEvent, item: ITag) {
    if (e.key === 't') {
      this.onClickItem(item);
    }
  }
}
