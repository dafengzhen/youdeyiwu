import { Injectable } from '@angular/core';
import { ITag } from '@/src/types';
import { HttpClient } from '@angular/common/http';

@Injectable({
  providedIn: 'root',
})
export class TagService {
  constructor(private httpClient: HttpClient) {}

  selectAll() {
    return this.httpClient.get<ITag[]>('/tags/select-all');
  }
}
