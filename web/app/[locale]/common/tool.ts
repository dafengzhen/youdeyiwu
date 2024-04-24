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

export const convertStyles = (styles: string) => {
  const filteredStyles = styles
    .split(';')
    .map((style) => style.trim())
    .filter((style) => style.length > 0);

  return filteredStyles.reduce(
    (acc, style) => {
      const [property, value] = style.split(':').map((part) => part.trim());
      const camelCasedProperty = property.replace(/-([a-z])/g, (g) =>
        g[1].toUpperCase(),
      );
      acc[camelCasedProperty] = value;
      return acc;
    },
    {} as Record<string, string>,
  );
};
