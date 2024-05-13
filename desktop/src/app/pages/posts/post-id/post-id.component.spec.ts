import { ComponentFixture, TestBed } from '@angular/core/testing';

import { PostIdComponent } from './post-id.component';

describe('PostIdComponent', () => {
  let component: PostIdComponent;
  let fixture: ComponentFixture<PostIdComponent>;

  beforeEach(async () => {
    await TestBed.configureTestingModule({
      imports: [PostIdComponent]
    })
    .compileComponents();
    
    fixture = TestBed.createComponent(PostIdComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
