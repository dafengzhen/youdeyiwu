import { Injectable } from '@angular/core';
import { HttpClient } from '@angular/common/http';
import {
  ILoginVariables,
  IRegisterVariables,
  IToken,
  IUser,
} from '@/src/types';

@Injectable({
  providedIn: 'root',
})
export class UserService {
  constructor(private httpClient: HttpClient) {}

  login(variables: ILoginVariables) {
    return this.httpClient.post<IToken>('/users/login', variables);
  }

  loginInfo() {
    return this.httpClient.get<IUser | null>('/users/login-info');
  }

  register(variables: IRegisterVariables) {
    return this.httpClient.post<IToken>('/users/register', variables);
  }

  logout() {
    return this.httpClient.post<void>('/users/logout', null);
  }
}
