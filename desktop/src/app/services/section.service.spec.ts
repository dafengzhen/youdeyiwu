import { TestBed } from '@angular/core/testing';

import { SectionService } from './section.service';

describe('SectionService', () => {
  let service: SectionService;

  beforeEach(() => {
    TestBed.configureTestingModule({});
    service = TestBed.inject(SectionService);
  });

  it('should be created', () => {
    expect(service).toBeTruthy();
  });
});
