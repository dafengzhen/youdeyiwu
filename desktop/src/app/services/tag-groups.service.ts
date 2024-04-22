import { Injectable } from '@angular/core';
import { HttpClient } from '@angular/common/http';
import { ITagGroup } from '@/src/types';

@Injectable({
  providedIn: 'root',
})
export class TagGroupsService {
  constructor(private httpClient: HttpClient) {}

  selectAll() {
    return this.httpClient.get<ITagGroup[]>('/tag-groups/select-all');
  }
}
