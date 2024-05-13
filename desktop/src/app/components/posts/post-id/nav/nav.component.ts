import { Component } from '@angular/core';
import { RouterLink } from '@angular/router';
import { Location } from '@angular/common';

@Component({
  selector: 'app-post-id-nav',
  standalone: true,
  imports: [RouterLink],
  templateUrl: './nav.component.html',
  styleUrl: './nav.component.scss',
})
export class NavComponent {
  constructor(private location: Location) {}

  onClickBack() {
    this.location.back();
  }

  onClickBackKeyup(e: KeyboardEvent) {
    if (e.key === 'r') {
      this.onClickBack();
    }
  }
}
