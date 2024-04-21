import { Injectable } from '@angular/core';
import { HttpClient } from '@angular/common/http';
import { IPage, IPost, TQueryParams } from '../../types';

@Injectable({
  providedIn: 'root',
})
export class PostService {
  constructor(private httpClient: HttpClient) {}

  selectAll(params?: TQueryParams) {
    return this.httpClient.get<IPage<IPost[]>>('/posts/select-all', {
      params,
    });
  }
}
