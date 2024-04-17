import { Injectable } from '@angular/core';
import { HttpClient } from '@angular/common/http';
import { IPage, IPost } from '../../types';

@Injectable({
  providedIn: 'root',
})
export class PostService {
  constructor(private httpClient: HttpClient) {}

  selectAll() {
    return this.httpClient.get<IPage<IPost[]>>('/posts/select-all');
  }
}
