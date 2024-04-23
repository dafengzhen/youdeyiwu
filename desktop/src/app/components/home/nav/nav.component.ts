import { Component, EventEmitter, Input, Output } from '@angular/core';

@Component({
  selector: 'app-home-nav',
  standalone: true,
  imports: [],
  templateUrl: './nav.component.html',
  styleUrl: './nav.component.scss',
})
export class NavComponent {
  @Input() name: string | undefined;
  @Input() currentName: string | undefined;
  @Input() useCreateArticleIcon: boolean | undefined;

  @Output() clickEvent = new EventEmitter();

  onClick() {
    this.clickEvent.emit();
  }
}
