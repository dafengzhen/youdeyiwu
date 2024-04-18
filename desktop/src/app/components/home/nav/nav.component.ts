import { Component, Input } from '@angular/core';

@Component({
  selector: 'app-home-nav',
  standalone: true,
  imports: [],
  templateUrl: './nav.component.html',
  styleUrl: './nav.component.scss',
})
export class NavComponent {
  @Input() name: string | undefined;
}
