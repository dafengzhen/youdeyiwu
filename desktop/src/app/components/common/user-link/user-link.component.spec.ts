import { ComponentFixture, TestBed } from '@angular/core/testing';

import { UserLinkComponent } from './user-link.component';

describe('UserLinkComponent', () => {
  let component: UserLinkComponent;
  let fixture: ComponentFixture<UserLinkComponent>;

  beforeEach(async () => {
    await TestBed.configureTestingModule({
      imports: [UserLinkComponent]
    })
    .compileComponents();
    
    fixture = TestBed.createComponent(UserLinkComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
