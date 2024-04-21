import { Pipe, PipeTransform } from '@angular/core';
import { IUser, IUserDetails } from '@/src/types';
import { getUserAlias } from '@/src/app/tools';

@Pipe({
  name: 'userAlias',
  standalone: true,
})
export class UserAliasPipe implements PipeTransform {
  transform(value?: IUser | IUserDetails | null, ...args: unknown[]): unknown {
    return getUserAlias(value);
  }
}
