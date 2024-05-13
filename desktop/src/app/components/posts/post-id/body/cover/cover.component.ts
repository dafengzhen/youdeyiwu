import { AfterViewInit, Component, ElementRef, ViewChild } from '@angular/core';

@Component({
  selector: 'app-post-id-body-cover',
  standalone: true,
  imports: [],
  templateUrl: './cover.component.html',
  styleUrl: './cover.component.scss',
})
export class CoverComponent implements AfterViewInit {
  hover = false;

  @ViewChild('rotatingCover') rotatingCover?: ElementRef<HTMLAnchorElement>;

  ngAfterViewInit() {
    const coverElement = this.rotatingCover;
    if (coverElement) {
      coverElement.nativeElement.addEventListener('mouseover', () => {
        coverElement.nativeElement.style.transform = `rotateZ(${this.hover ? -5 : 5}deg)`;
        this.hover = !this.hover;
      });
    }
  }
}
