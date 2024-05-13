import { ComponentFixture, TestBed } from '@angular/core/testing';

import { ShortcutBtnComponent } from './shortcut-btn.component';

describe('ShortcutBtnComponent', () => {
  let component: ShortcutBtnComponent;
  let fixture: ComponentFixture<ShortcutBtnComponent>;

  beforeEach(async () => {
    await TestBed.configureTestingModule({
      imports: [ShortcutBtnComponent]
    })
    .compileComponents();
    
    fixture = TestBed.createComponent(ShortcutBtnComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
