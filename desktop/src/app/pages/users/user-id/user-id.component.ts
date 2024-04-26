import { Component, OnInit } from '@angular/core';
import { ActivatedRoute, Router } from '@angular/router';

@Component({
  selector: 'app-user-id',
  standalone: true,
  imports: [],
  templateUrl: './user-id.component.html',
  styleUrl: './user-id.component.scss',
})
export class UserIdComponent implements OnInit {
  id?: number;

  constructor(
    private route: ActivatedRoute,
    private router: Router,
  ) {}

  ngOnInit() {
    const id = this.route.snapshot.paramMap.get('id');

    // this.router.navigate(['/heroes', { id: this.id }]);
    console.log(id);
  }
}
