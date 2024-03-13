import { createContext } from 'react';
import { IUser, IUserDetails } from '@/app/[locale]/interfaces/users';

export const UserIdContext = createContext<{
  details?: IUserDetails;
  currentUser?: IUser | null;
}>({});
