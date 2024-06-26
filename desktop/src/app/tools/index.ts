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

export const generateRandomNumber = (min: number, max: number) => {
  if (min > max) {
    [min, max] = [max, min];
  }
  return Math.floor(Math.random() * (max - min + 1)) + min;
};

export const parseURL = (url: string, baseURL?: string) => {
  const parsedUrl = new URL(url, baseURL ?? location.origin);
  const searchParams = new URLSearchParams(parsedUrl.search);

  const queryParams: Record<string, string> = {};
  searchParams.forEach((value, key) => (queryParams[key] = value));

  return {
    pathname: parsedUrl.pathname,
    search: parsedUrl.search,
    queryParams,
  };
};
