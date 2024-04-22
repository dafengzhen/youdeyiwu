import { Component, EventEmitter, Input, Output } from '@angular/core';
import { IPageable } from '@/src/types';

@Component({
  selector: 'app-home-footer',
  standalone: true,
  imports: [],
  templateUrl: './footer.component.html',
  styleUrl: './footer.component.scss',
})
export class FooterComponent {
  @Input() pageable?: IPageable;
  @Input() showPageable!: boolean;
  @Input() showBackBtn!: boolean;
  @Input() showXBtn!: boolean;

  @Output() xBtnEvent = new EventEmitter();
  @Output() backBtnEvent = new EventEmitter();
  @Output() previousPageEvent = new EventEmitter<number>();
  @Output() nextPageEvent = new EventEmitter<number>();
  @Output() pageEvent = new EventEmitter<number>();

  onClickPreviousPage(e: MouseEvent) {
    e.preventDefault();
    e.stopPropagation();

    if (!this.pageable?.previous) {
      return;
    }

    this.previousPageEvent.emit(Math.max(this.pageable.page - 1, 0));
  }

  onClickNextPage(e: MouseEvent) {
    e.preventDefault();
    e.stopPropagation();

    if (!this.pageable?.next) {
      return;
    }

    this.nextPageEvent.emit(
      Math.min(this.pageable.page + 1, this.pageable.pages),
    );
  }

  onClickPage(e: MouseEvent) {
    e.preventDefault();
    e.stopPropagation();

    this.pageEvent.emit(this.pageable?.page ?? 0);
  }

  onClickBackBtn(e: MouseEvent) {
    e.preventDefault();
    e.stopPropagation();

    this.backBtnEvent.emit();
  }

  onClickXBtn(e: MouseEvent) {
    e.preventDefault();
    e.stopPropagation();

    this.xBtnEvent.emit();
  }
}
