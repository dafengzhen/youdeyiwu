import { ComponentFixture, TestBed } from '@angular/core/testing';

import { UserIdComponent } from './user-id.component';

describe('UserIdComponent', () => {
  let component: UserIdComponent;
  let fixture: ComponentFixture<UserIdComponent>;

  beforeEach(async () => {
    await TestBed.configureTestingModule({
      imports: [UserIdComponent]
    })
    .compileComponents();
    
    fixture = TestBed.createComponent(UserIdComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
