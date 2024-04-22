import { Component, EventEmitter, Output } from '@angular/core';
import { IError, ISectionGroup } from '@/src/types';
import { injectQuery } from '@tanstack/angular-query-experimental';
import { lastValueFrom } from 'rxjs';
import { SectionGroupsService } from '@/src/app/services/section-groups.service';

@Component({
  selector: 'app-home-section-groups',
  standalone: true,
  imports: [],
  templateUrl: './section-groups.component.html',
  styleUrl: './section-groups.component.scss',
})
export class SectionGroupsComponent {
  @Output() itemEvent = new EventEmitter<ISectionGroup>();

  sectionGroupsQuery = injectQuery<ISectionGroup[], IError>(() => ({
    queryKey: ['/section-groups/select-all'],
    queryFn: () => {
      return lastValueFrom(this.sectionGroupsService.selectAll());
    },
  }));

  constructor(private sectionGroupsService: SectionGroupsService) {}

  onClickItem(item: ISectionGroup) {
    this.itemEvent.emit(item);
  }

  onKeyup(e: KeyboardEvent, item: ISectionGroup) {
    if (e.key === 'g') {
      this.onClickItem(item);
    }
  }
}
