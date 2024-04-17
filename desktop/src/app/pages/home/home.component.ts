import { AfterViewInit, Component, ElementRef, ViewChild } from '@angular/core';
import { NgOptimizedImage } from '@angular/common';
import { PostService } from '../../services/post.service';

@Component({
  selector: 'app-home',
  standalone: true,
  imports: [NgOptimizedImage],
  templateUrl: './home.component.html',
  styleUrl: './home.component.scss',
})
export class HomeComponent implements AfterViewInit {
  @ViewChild('myDiv') myDiv: ElementRef | undefined;

  constructor(private postService: PostService) {
    this.postService.selectAll().subscribe((value) => {
      console.log(value);
    });
  }

  ngAfterViewInit(): void {
    this.checkScrollbar();
  }

  checkScrollbar(): void {
    if (!this.myDiv) {
      return;
    }

    const hasScrollbar =
      this.myDiv.nativeElement.scrollHeight >
      this.myDiv.nativeElement.clientHeight;
    if (hasScrollbar) {
      this.myDiv.nativeElement.classList.add('yw-home-margin-right');
    } else {
      this.myDiv.nativeElement.classList.remove('yw-home-margin-right');
    }
  }
}
