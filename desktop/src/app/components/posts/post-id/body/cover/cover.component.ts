import {
  AfterViewInit,
  Component,
  ElementRef,
  Input,
  OnDestroy,
  ViewChild,
} from '@angular/core';
import { NgOptimizedImage } from '@angular/common';
import { IPostDetails } from '@/src/types';

@Component({
  selector: 'app-post-id-body-cover',
  standalone: true,
  imports: [NgOptimizedImage],
  templateUrl: './cover.component.html',
  styleUrl: './cover.component.scss',
})
export class CoverComponent implements AfterViewInit, OnDestroy {
  @ViewChild('rotatingCover') rotatingCover?: ElementRef<HTMLAnchorElement>;

  hover = false;

  handleMouseover = this.rotateCoverOnHover.bind(this);

  @Input() pending: boolean = false;
  @Input() details!: IPostDetails;

  ngAfterViewInit(): void {
    const coverElement = this.rotatingCover;
    coverElement?.nativeElement.addEventListener(
      'mouseover',
      this.handleMouseover,
    );
  }

  ngOnDestroy(): void {
    const coverElement = this.rotatingCover;
    coverElement?.nativeElement.removeEventListener(
      'mouseover',
      this.handleMouseover,
    );
  }

  rotateCoverOnHover() {
    const coverElement = this.rotatingCover;
    if (coverElement) {
      coverElement.nativeElement.style.transform = `rotateZ(${this.hover ? -5 : 5}deg)`;
      this.hover = !this.hover;
    }
  }
}
