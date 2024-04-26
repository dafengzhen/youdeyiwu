import { IUser, IUserDetails } from '@/src/types';
import { formatDistanceStrict, isToday } from 'date-fns';

export const getUserAlias = (user?: IUser | IUserDetails | null) => {
  if (!user) {
    return 'Anonymous';
  }

  return user.alias ?? user.username ?? 'Unknown';
};

export const fromNow = (value: string): string => {
  return formatDistanceStrict(new Date(), new Date(value));
};

export const isNew = (value: string): boolean => {
  return isToday(value);
};

export const createSequenceArray = (length: number) => {
  return Array.from({ length }, (_, index) => index + 1);
};
