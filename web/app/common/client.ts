'use client';

import format from 'date-fns/format';
import type { IUser } from '@/app/interfaces/users';
import { formatDistanceStrict } from 'date-fns';

export const isHttpOrHttps = (value?: string) => {
  return value && (value.startsWith('http') || value.startsWith('https'));
};

export const getFormattedTime = (time: string) => {
  const giveDate = new Date(time);
  return format(
    giveDate,
    new Date().getFullYear() === giveDate.getFullYear()
      ? 'MM-dd'
      : 'yyyy-MM-dd',
  );
};

export const formatCurrentDateTime = (date?: Date, f?: string) => {
  return format(date ?? new Date(), f ?? 'yyyy-MM-dd HH:mm:ss');
};

export const compressHTML = (html: string) => {
  return html
    .replace(/<!--[\s\S]*?-->/g, '')
    .replace(/\s+/g, ' ')
    .replace(/>\s+</g, '><');
};

export const capitalizeFirstLetter = (value: string) => {
  value = value.toLowerCase();
  return value.charAt(0).toUpperCase() + value.slice(1);
};

export const convertToCamelCase = (value: string, separator = '') => {
  return value
    .split('_')
    .map((word, index) => {
      if (index === 0) {
        return word.toLowerCase();
      }
      return word.charAt(0).toUpperCase() + word.slice(1).toLowerCase();
    })
    .join(separator);
};

export const removeDuplicatesByProperty = (arr: any[], property: string) => {
  const uniqueValues = new Set();
  const result = [];

  for (let i = 0; i < arr.length; i++) {
    const value = arr[i][property];
    if (!uniqueValues.has(value)) {
      uniqueValues.add(value);
      result.push(arr[i]);
    }
  }

  return result;
};

export const nonNum = (value: string) => {
  return !/^\d+$/.test(value);
};

export const trimObjectStrings = (obj: Record<string, any>) => {
  const trimmedObj: Record<string, any> = {};

  for (let key in obj) {
    if (obj.hasOwnProperty(key)) {
      const value = obj[key];
      if (typeof value === 'string') {
        trimmedObj[key] = value.trim();
      } else {
        trimmedObj[key] = value;
      }
    }
  }

  return trimmedObj;
};

export const getUserAlias = (user?: IUser | null) => {
  if (!user) {
    return 'Anonymous';
  }

  return user.alias ?? user.username;
};

export const fromNow = (datetime: string): string => {
  return formatDistanceStrict(new Date(), new Date(datetime));
};

export const formatCount = (count: number | null | undefined): string => {
  if (typeof count !== 'number') {
    return '';
  }

  if (count === 0) {
    return '';
  } else if (count < 1000) {
    return count + '';
  }

  let suffix = '';
  let divisor = 1;

  if (count >= 1000000000) {
    suffix = 'B';
    divisor = 1000000000;
  } else if (count >= 1000000) {
    suffix = 'M';
    divisor = 1000000;
  } else if (count >= 1000) {
    suffix = 'K';
    divisor = 1000;
  }

  return (count / divisor).toFixed(1) + suffix;
};

export const wait = async (ms = 1000) => {
  return new Promise((resolve) => {
    setTimeout(() => {
      resolve(true);
    }, ms);
  });
};
