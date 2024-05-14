import { Component, Input } from '@angular/core';
import { IPostDetails } from '@/src/types';

@Component({
  selector: 'app-post-id-body-name',
  standalone: true,
  imports: [],
  templateUrl: './name.component.html',
  styleUrl: './name.component.scss',
})
export class NameComponent {
  @Input() details!: IPostDetails;
}
