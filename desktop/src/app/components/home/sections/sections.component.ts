import { Component, EventEmitter, Output } from '@angular/core';
import { IError, ISection } from '@/src/types';
import { injectQuery } from '@tanstack/angular-query-experimental';
import { lastValueFrom } from 'rxjs';
import { SectionService } from '@/src/app/services/section.service';

@Component({
  selector: 'app-home-sections',
  standalone: true,
  imports: [],
  templateUrl: './sections.component.html',
  styleUrl: './sections.component.scss',
})
export class SectionsComponent {
  @Output() itemEvent = new EventEmitter<ISection>();

  sectionsQuery = injectQuery<ISection[], IError>(() => ({
    queryKey: ['/sections/select-all'],
    queryFn: () => {
      return lastValueFrom(this.sectionService.selectAll());
    },
  }));

  constructor(private sectionService: SectionService) {}

  onClickItem(item: ISection) {
    this.itemEvent.emit(item);
  }

  onKeyup(e: KeyboardEvent, item: ISection) {
    if (e.key === 's') {
      this.onClickItem(item);
    }
  }
}
