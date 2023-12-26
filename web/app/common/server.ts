'use server';

import { cookies, headers } from 'next/headers';
import { SECURE_TK, TK } from '@/app/constants';
import { redirect } from 'next/navigation';
import type { IToken, TQueryParams } from '@/app/interfaces';
import type { IUser } from '@/app/interfaces/users';
import type { Metadata } from 'next';

export const obtainCredentials = (checkLogin?: boolean) => {
  const isHttpsSite = process.env.IS_HTTPS_SITE === 'true';
  const cookie = cookies().get(isHttpsSite ? SECURE_TK : TK);

  if (!cookie) {
    if (checkLogin) {
      redirect('/login');
    } else {
      return '';
    }
  }

  return cookie.value;
};

export const deleteTicket = () => {
  const isHttpsSite = process.env.IS_HTTPS_SITE === 'true';
  cookies().delete(isHttpsSite ? SECURE_TK : TK);
};

export const getQueryParams = (init?: TQueryParams) => {
  return new URLSearchParams(init).toString();
};

export const checkResponseStatus = (status: number) => {
  if (status === 401) {
    redirect('/login');
  }
};

export const isNum = (value: string) => {
  return /^\d+$/.test(value);
};

export const parseNum = (num: string | undefined | null) => {
  return num && isNum(num) ? parseInt(num) : null;
};

export const excludeId = (source: Record<string, any>) => {
  const { id, ..._variables } = Object.assign({}, source);
  return { id, _variables };
};

export const convertToCookieExpiration = (days: number) => {
  return new Date(new Date().getTime() + days * 24 * 60 * 60 * 1000);
};

export const processFirstCharacter = (username: string) => {
  let firstChar = username.substring(0, 1);
  if (/^[a-zA-Z]/.test(firstChar)) {
    firstChar = firstChar.toUpperCase();
  }
  return firstChar;
};

export const daysToSeconds = (days: number) => {
  return days * 24 * 60 * 60;
};

export const setCredentials = (data: IToken) => {
  const isHttpsSite = process.env.IS_HTTPS_SITE === 'true';
  cookies().set(isHttpsSite ? SECURE_TK : TK, data.token, {
    path: '/',
    httpOnly: true,
    sameSite: 'strict',
    secure: isHttpsSite,
    maxAge:
      typeof data.expDays === 'number'
        ? daysToSeconds(data.expDays)
        : undefined,
  });
};

export const clearCredentials = () => {
  const isHttpsSite = process.env.IS_HTTPS_SITE === 'true';
  cookies().set(isHttpsSite ? SECURE_TK : TK, '', {
    path: '/',
    maxAge: 0,
  });
};

export const getUserAlias = (user?: IUser | null) => {
  if (!user) {
    return 'Anonymous';
  }

  return user.alias ?? user.username;
};

export const getXRealIp = () => {
  let xRealIp = headers().get('x-real-ip');
  const xForwardedFor = headers().get('x-forwarded-for');

  if (!xRealIp && xForwardedFor) {
    xRealIp = xForwardedFor?.split(',').at(0) ?? 'Unknown';
  }

  return xRealIp;
};

export const isHttpOrHttps = (value?: string) => {
  return value && (value.startsWith('http') || value.startsWith('https'));
};

export const errorTitle = (e?: any): Metadata => {
  const title = 'Sorry, an error has occurred';
  return {
    title,
    description: e?.message ?? title,
  };
};

export const errorContent = (e?: any): string => {
  return e?.message ?? 'Sorry, an error has occurred';
};
