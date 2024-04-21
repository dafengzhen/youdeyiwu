import { ComponentFixture, TestBed } from '@angular/core/testing';

import { PostsHeaderComponent } from './header.component';

describe('HeaderComponent', () => {
  let component: PostsHeaderComponent;
  let fixture: ComponentFixture<PostsHeaderComponent>;

  beforeEach(async () => {
    await TestBed.configureTestingModule({
      imports: [PostsHeaderComponent],
    }).compileComponents();

    fixture = TestBed.createComponent(PostsHeaderComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
