import { IUser, IUserDetails } from '@/src/types';
import { formatDistanceStrict } from 'date-fns';

export const getUserAlias = (user?: IUser | IUserDetails | null) => {
  if (!user) {
    return 'Anonymous';
  }

  return user.alias ?? user.username ?? 'Unknown';
};

export const fromNow = (datetime: string): string => {
  return formatDistanceStrict(new Date(), new Date(datetime));
};
