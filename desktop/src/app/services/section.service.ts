import { Injectable } from '@angular/core';
import { HttpClient } from '@angular/common/http';
import { ISection } from '@/src/types';

@Injectable({
  providedIn: 'root',
})
export class SectionService {
  constructor(private httpClient: HttpClient) {}

  selectAll() {
    return this.httpClient.get<ISection[]>('/sections/select-all');
  }
}
