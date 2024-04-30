import { Component, EventEmitter, Input, Output } from '@angular/core';
import {
  FormControl,
  FormGroup,
  FormsModule,
  ReactiveFormsModule,
  Validators,
} from '@angular/forms';
import { injectMutation } from '@tanstack/angular-query-experimental';
import { UserService } from '@/src/app/services/user.service';
import {
  IError,
  ILoginVariables,
  IRegisterVariables,
  IToken,
} from '@/src/types';
import { delay, lastValueFrom } from 'rxjs';
import { SECURE_TK, TK } from '@/src/app/constants';
import { environment } from '@/src/environments/environment';
import { CookieService } from 'ngx-cookie-service';
import { generateRandomNumber } from '@/src/app/tools';

@Component({
  selector: 'app-login',
  standalone: true,
  imports: [FormsModule, ReactiveFormsModule],
  templateUrl: './login.component.html',
  styleUrl: './login.component.scss',
})
export class LoginComponent {
  @Input() showLoginPage = true;

  @Output() closeEvent = new EventEmitter();
  @Output() successfulEvent = new EventEmitter();

  successful = false;
  progress = 0;
  intervalId?: number;
  isLoading = false;

  form = new FormGroup({
    username: new FormControl('', Validators.required),
    password: new FormControl('', [Validators.required]),
  });

  loginMutation = injectMutation<IToken, IError, ILoginVariables>(() => ({
    mutationFn: (variables) =>
      lastValueFrom(this.userService.login(variables).pipe(delay(5000))),
  }));

  registerMutation = injectMutation<IToken, IError, IRegisterVariables>(() => ({
    mutationFn: (variables) =>
      lastValueFrom(this.userService.register(variables)),
  }));

  constructor(
    private userService: UserService,
    private cookieService: CookieService,
  ) {}

  get username() {
    return this.form.get('username');
  }

  get password() {
    return this.form.get('password');
  }

  async handleSubmit() {
    try {
      const username = this.form.value.username;
      const password = this.form.value.password;

      if (!username || !password) {
        alert('The username or password is not filled in.');
        return;
      }

      this.isLoading = true;
      this.intervalId = setInterval(() => {
        this.progress =
          this.progress >= 90
            ? generateRandomNumber(50, 90)
            : this.progress + 5;
      }, 100);

      let data;
      if (this.showLoginPage) {
        data = await this.loginMutation.mutateAsync({
          username,
          password,
        });
      } else {
        data = await this.registerMutation.mutateAsync({
          username,
          password,
        });
      }

      this.progress = 100;
      this.setCredentials(data);
      this.successful = true;
      this.successfulEvent.emit();
      const message = this.showLoginPage
        ? 'Login successful.'
        : 'Successful registration.';

      setTimeout(() => {
        alert(message);
      }, 1000);
    } catch (e) {
      const message = (e as IError).error.message;
      alert(message);
    } finally {
      clearInterval(this.intervalId);
      this.isLoading = false;
    }
  }

  onClickToggle() {
    this.showLoginPage = !this.showLoginPage;
  }

  setCredentials(data: IToken) {
    const isHttpsSite = environment.isHttpsSite;
    this.cookieService.set(isHttpsSite ? SECURE_TK : TK, data.token, {
      path: '/',
      sameSite: 'Strict',
      secure: isHttpsSite,
      expires:
        typeof data.expDays === 'number'
          ? data.expDays * 24 * 60 * 60
          : undefined,
    });
  }

  onClickClose() {
    this.closeEvent.emit();
  }
}
