import { Component, Input } from '@angular/core';
import { ActivatedRoute, Router, RouterLink } from '@angular/router';
import { IPostDetails } from '@/src/types';

@Component({
  selector: 'app-post-id-nav',
  standalone: true,
  imports: [RouterLink],
  templateUrl: './nav.component.html',
  styleUrl: './nav.component.scss',
})
export class NavComponent {
  @Input() pending: boolean = false;
  @Input() details!: IPostDetails;
  @Input() previousPageId?: string | null;
  @Input() nextPageId?: string | null;

  currentId?: string | null;

  constructor(
    private router: Router,
    private route: ActivatedRoute,
  ) {
    this.currentId = this.route.snapshot.paramMap.get('id');
  }

  onClickBack() {
    this.router.navigate(['/']);
  }

  onClickBackKeyup(e: KeyboardEvent) {
    if (e.key === 'r') {
      this.onClickBack();
    }
  }

  onClickPreviousPage(previousPageId?: string | null) {
    this.currentId = previousPageId;
  }

  onClickNextPage(nextPageId?: string | null) {
    this.currentId = nextPageId;
  }
}
