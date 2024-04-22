import { Component, EventEmitter, Output } from '@angular/core';
import { IError, ITagGroup } from '@/src/types';
import { injectQuery } from '@tanstack/angular-query-experimental';
import { lastValueFrom } from 'rxjs';
import { TagGroupsService } from '@/src/app/services/tag-groups.service';

@Component({
  selector: 'app-home-tag-groups',
  standalone: true,
  imports: [],
  templateUrl: './tag-groups.component.html',
  styleUrl: './tag-groups.component.scss',
})
export class TagGroupsComponent {
  @Output() itemEvent = new EventEmitter<ITagGroup>();

  tagGroupsQuery = injectQuery<ITagGroup[], IError>(() => ({
    queryKey: ['/tag-groups/select-all'],
    queryFn: () => {
      return lastValueFrom(this.tagGroupsService.selectAll());
    },
  }));

  constructor(private tagGroupsService: TagGroupsService) {}

  onClickItem(item: ITagGroup) {
    this.itemEvent.emit(item);
  }

  onKeyup(e: KeyboardEvent, item: ITagGroup) {
    if (e.key === 'g') {
      this.onClickItem(item);
    }
  }
}
