import { Component } from '@angular/core';
import { NgOptimizedImage } from '@angular/common';

@Component({
  selector: 'app-post-id-head',
  standalone: true,
  imports: [NgOptimizedImage],
  templateUrl: './head.component.html',
  styleUrl: './head.component.scss',
})
export class HeadComponent {}
