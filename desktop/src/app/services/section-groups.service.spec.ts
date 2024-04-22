import { TestBed } from '@angular/core/testing';

import { SectionGroupsService } from './section-groups.service';

describe('SectionGroupsService', () => {
  let service: SectionGroupsService;

  beforeEach(() => {
    TestBed.configureTestingModule({});
    service = TestBed.inject(SectionGroupsService);
  });

  it('should be created', () => {
    expect(service).toBeTruthy();
  });
});
