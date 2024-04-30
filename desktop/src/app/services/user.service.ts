import { Injectable } from '@angular/core';
import { HttpClient } from '@angular/common/http';
import { ILoginVariables, IRegisterVariables, IToken } from '@/src/types';

@Injectable({
  providedIn: 'root',
})
export class UserService {
  constructor(private httpClient: HttpClient) {}

  login(variables: ILoginVariables) {
    return this.httpClient.post<IToken>('/users/login', variables);
  }

  register(variables: IRegisterVariables) {
    return this.httpClient.post<IToken>('/users/register', variables);
  }
}
