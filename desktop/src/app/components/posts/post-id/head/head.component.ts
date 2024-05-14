import { Component, Input } from '@angular/core';
import { NgOptimizedImage } from '@angular/common';
import { IPostDetails } from '@/src/types';
import { RouterLink } from '@angular/router';

@Component({
  selector: 'app-post-id-head',
  standalone: true,
  imports: [NgOptimizedImage, RouterLink],
  templateUrl: './head.component.html',
  styleUrl: './head.component.scss',
})
export class HeadComponent {
  @Input() pending: boolean = false;
  @Input() details!: IPostDetails;
}
