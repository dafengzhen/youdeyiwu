import { Component } from '@angular/core';
import { NavComponent } from '@/src/app/components/posts/post-id/nav/nav.component';
import { BodyComponent } from '@/src/app/components/posts/post-id/body/body.component';
import { CoverComponent } from '@/src/app/components/posts/post-id/body/cover/cover.component';
import { HeadComponent } from '@/src/app/components/posts/post-id/head/head.component';
import { IShortcutBtnComponentClickEvent } from '@/src/app/components/posts/post-id/body/shortcut-btn/shortcut-btn.component';

@Component({
  selector: 'app-post-id',
  standalone: true,
  imports: [NavComponent, BodyComponent, CoverComponent, HeadComponent],
  templateUrl: './post-id.component.html',
  styleUrl: './post-id.component.scss',
})
export class PostIdComponent {
  expand = true;

  onShortcutBtnClickEvent(e: IShortcutBtnComponentClickEvent) {
    if (e.type === 'cover') {
      this.expand = e.value;
    }
  }
}
