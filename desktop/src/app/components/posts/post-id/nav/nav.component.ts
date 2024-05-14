import { Component, Input, OnInit } from '@angular/core';
import {
  ActivatedRoute,
  NavigationEnd,
  Router,
  RouterLink,
} from '@angular/router';
import { Location } from '@angular/common';
import { IPostDetails } from '@/src/types';

@Component({
  selector: 'app-post-id-nav',
  standalone: true,
  imports: [RouterLink],
  templateUrl: './nav.component.html',
  styleUrl: './nav.component.scss',
})
export class NavComponent implements OnInit {
  isDestroy = false;

  @Input() pending: boolean = false;
  @Input() details!: IPostDetails;
  @Input() previousPageId?: string | null;
  @Input() nextPageId?: string | null;

  constructor(
    private location: Location,
    private route: ActivatedRoute,
    private router: Router,
  ) {}

  onClickBack() {
    this.location.back();
  }

  onClickBackKeyup(e: KeyboardEvent) {
    if (e.key === 'r') {
      this.onClickBack();
    }
  }

  onClickPreviousPage(e: MouseEvent) {
    e.stopPropagation();
    e.preventDefault();
  }

  ngOnInit(): void {
    this.router.events.subscribe((event) => {
      if (event instanceof NavigationEnd) {
        console.log('end');
        console.log(event);
      }
    });
  }
}
