import type { Metadata } from 'next';
import type { IUser, IUserDetails } from '@/app/[locale]/interfaces/users';

export const isNum = (value: string) => {
  return /^\d+$/.test(value);
};

export const incorrectMetadataTitle = (e?: any): Metadata => {
  const title = 'Sorry, an error has occurred';
  return {
    title,
    description: e?.message ?? title,
  };
};

export const getUserAlias = (user?: IUser | IUserDetails | null) => {
  if (!user) {
    return 'Anonymous';
  }

  return user.alias ?? user.username;
};

export const formatTitleForURL = (title: string) => {
  return title.trim().replace(/\s+/g, '-').toLowerCase();
};
