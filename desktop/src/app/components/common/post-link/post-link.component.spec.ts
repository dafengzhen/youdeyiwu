import { ComponentFixture, TestBed } from '@angular/core/testing';

import { PostLinkComponent } from './post-link.component';

describe('PostLinkComponent', () => {
  let component: PostLinkComponent;
  let fixture: ComponentFixture<PostLinkComponent>;

  beforeEach(async () => {
    await TestBed.configureTestingModule({
      imports: [PostLinkComponent]
    })
    .compileComponents();
    
    fixture = TestBed.createComponent(PostLinkComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
