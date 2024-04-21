import { ComponentFixture, TestBed } from '@angular/core/testing';

import { TagsHeaderComponent } from './header.component';

describe('HeaderComponent', () => {
  let component: TagsHeaderComponent;
  let fixture: ComponentFixture<TagsHeaderComponent>;

  beforeEach(async () => {
    await TestBed.configureTestingModule({
      imports: [TagsHeaderComponent],
    }).compileComponents();

    fixture = TestBed.createComponent(TagsHeaderComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
