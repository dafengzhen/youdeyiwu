import { TestBed } from '@angular/core/testing';

import { TagGroupsService } from './tag-groups.service';

describe('TagGroupsService', () => {
  let service: TagGroupsService;

  beforeEach(() => {
    TestBed.configureTestingModule({});
    service = TestBed.inject(TagGroupsService);
  });

  it('should be created', () => {
    expect(service).toBeTruthy();
  });
});
