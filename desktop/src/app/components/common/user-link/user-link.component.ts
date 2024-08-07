import { Component, Input } from '@angular/core';
import { UserAliasPipe } from '@/src/app/pipes/user-alias.pipe';
import { IUser, IUserDetails } from '@/src/types';
import { RouterLink } from '@angular/router';

@Component({
  selector: 'app-user-link',
  standalone: true,
  imports: [UserAliasPipe, RouterLink],
  templateUrl: './user-link.component.html',
  styleUrl: './user-link.component.scss',
})
export class UserLinkComponent {
  @Input() user?: IUser | IUserDetails | null;
}
