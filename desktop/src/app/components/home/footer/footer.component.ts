import {
  Component,
  EventEmitter,
  Input,
  Output,
  ViewChild,
} from '@angular/core';
import { IPageable } from '@/src/types';
import { RouterLink } from '@angular/router';
import { ModalComponent } from '@/src/app/components/common/modal/modal.component';
import { LoginComponent } from '@/src/app/components/common/login/login.component';

@Component({
  selector: 'app-home-footer',
  standalone: true,
  imports: [RouterLink, ModalComponent, LoginComponent],
  templateUrl: './footer.component.html',
  styleUrl: './footer.component.scss',
})
export class FooterComponent {
  @ViewChild(ModalComponent) modal?: ModalComponent;

  @Input() pageable?: IPageable;
  @Input() showPageable!: boolean;
  @Input() showBackBtn!: boolean;
  @Input() showXBtn!: boolean;

  @Output() xBtnEvent = new EventEmitter();
  @Output() backBtnEvent = new EventEmitter();
  @Output() previousPageEvent = new EventEmitter<number>();
  @Output() nextPageEvent = new EventEmitter<number>();
  @Output() pageEvent = new EventEmitter<number>();

  showLoginPage = true;

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

  onClickLogin(showLoginPage: boolean, e: MouseEvent) {
    e.stopPropagation();
    e.preventDefault();

    this.showLoginPage = showLoginPage;
    this.modal?.show();
  }

  onShownBsModalEvent() {
    this.modal?.handleUpdate();
  }

  onCloseEventLogin() {
    this.modal?.hide();
  }
}
