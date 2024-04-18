import { AfterViewInit, Component, ElementRef, ViewChild } from '@angular/core';
import { NavComponent } from '@/src/app/components/home/nav/nav.component';
import { HeaderComponent } from '@/src/app/components/home/header/header.component';
import { FooterComponent } from '@/src/app/components/home/footer/footer.component';
import { BodyComponent } from '@/src/app/components/home/body/body.component';

@Component({
  selector: 'app-home',
  standalone: true,
  imports: [NavComponent, HeaderComponent, FooterComponent, BodyComponent],
  templateUrl: './home.component.html',
  styleUrl: './home.component.scss',
})
export class HomeComponent implements AfterViewInit {
  @ViewChild('myDiv') myDiv: ElementRef | undefined;

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
