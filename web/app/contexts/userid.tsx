import { createContext } from 'react';
import { IUser, IUserDetails } from '@/app/interfaces/users';

export const UserIdContext = createContext<{
  details?: IUserDetails;
  currentUser?: IUser | null;
}>({});
