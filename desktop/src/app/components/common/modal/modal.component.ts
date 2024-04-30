import {
  AfterViewInit,
  Component,
  ElementRef,
  EventEmitter,
  Input,
  OnDestroy,
  Output,
  ViewChild,
} from '@angular/core';
import { NgTemplateOutlet } from '@angular/common';
import * as bootstrap from 'bootstrap';

@Component({
  selector: 'app-modal',
  standalone: true,
  imports: [NgTemplateOutlet],
  templateUrl: './modal.component.html',
  styleUrl: './modal.component.scss',
})
export class ModalComponent implements AfterViewInit, OnDestroy {
  @ViewChild('modal') modalElement?: ElementRef<HTMLDivElement>;

  @Input() customLayout = false;
  @Input() defaultLayout = false;
  @Input() sm = false;

  @Output() shownBsModalEvent = new EventEmitter();
  @Output() hiddenBsModalEvent = new EventEmitter();

  constructor() {}

  shownBsModal() {
    this.handleUpdate();
    this.shownBsModalEvent.emit();
  }

  hiddenBsModal() {
    this.hiddenBsModalEvent.emit();
  }

  getModal() {
    const element = this.modalElement;
    if (!element) {
      return;
    }

    return bootstrap.Modal.getOrCreateInstance(element.nativeElement);
  }

  handleUpdate() {
    this.getModal()?.handleUpdate();
  }

  show() {
    this.getModal()?.show();
  }

  hide() {
    this.getModal()?.hide();
  }

  click() {
    this.modalElement?.nativeElement.click();
  }

  ngAfterViewInit(): void {
    const element = this.modalElement;
    if (!element) {
      return;
    }

    element.nativeElement.addEventListener(
      'shown.bs.modal',
      this.shownBsModal.bind(this),
    );
    element.nativeElement.addEventListener(
      'hidden.bs.modal',
      this.hiddenBsModal.bind(this),
    );
  }

  ngOnDestroy(): void {
    const element = this.modalElement;
    if (!element) {
      return;
    }

    element.nativeElement.removeEventListener(
      'shown.bs.modal',
      this.shownBsModal,
    );
    element.nativeElement.removeEventListener(
      'hidden.bs.modal',
      this.hiddenBsModal,
    );
  }
}
