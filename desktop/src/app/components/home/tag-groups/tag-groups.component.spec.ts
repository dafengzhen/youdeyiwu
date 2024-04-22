import { ComponentFixture, TestBed } from '@angular/core/testing';

import { TagGroupsComponent } from './tag-groups.component';

describe('TagGroupsComponent', () => {
  let component: TagGroupsComponent;
  let fixture: ComponentFixture<TagGroupsComponent>;

  beforeEach(async () => {
    await TestBed.configureTestingModule({
      imports: [TagGroupsComponent]
    })
    .compileComponents();
    
    fixture = TestBed.createComponent(TagGroupsComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
