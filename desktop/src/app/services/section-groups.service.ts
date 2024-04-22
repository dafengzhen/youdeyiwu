import { Injectable } from '@angular/core';
import { HttpClient } from '@angular/common/http';
import { ISectionGroup } from '@/src/types';

@Injectable({
  providedIn: 'root',
})
export class SectionGroupsService {
  constructor(private httpClient: HttpClient) {}

  selectAll() {
    return this.httpClient.get<ISectionGroup[]>('/section-groups/select-all');
  }
}
