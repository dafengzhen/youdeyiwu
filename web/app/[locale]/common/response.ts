import type {
  IErrorResponse,
  ISuccessResponse,
  IToken,
  TQueryParams,
} from '@/app/[locale]/interfaces';
import queryString from 'query-string';
import {
  AUTHORIZATION,
  BEARER,
  JSON_HEADER,
  SECURE_TK,
  TK,
} from '@/app/[locale]/constants';
import { cookies } from 'next/headers';

export const createSuccessResponse = <T>(
  data: T,
  message?: string,
): ISuccessResponse<T> => {
  return {
    data,
    message: message ?? 'OK',
    status: 200,
    isSuccess: true,
    isError: false,
  };
};

export const createErrorResponse = <T, E>(
  message: any,
  error?: E,
  data?: T,
  status?: number,
  code?: number,
): IErrorResponse<T, E> => {
  let _message;
  if (
    typeof message === 'string' ||
    typeof message === 'number' ||
    typeof message === 'boolean'
  ) {
    _message = message + '';
  } else if (typeof message === 'object') {
    if (typeof message.message === 'string') {
      _message = message.message;
    } else if (typeof message.error === 'string') {
      _message = message.error;
    }

    if (typeof message.status === 'number') {
      status = message.status;
    }
  } else {
    console.error(
      'Error: This error message may not be output correctly',
      message,
    );
    _message = 'Sorry. Unknown error';
  }

  return {
    data,
    error,
    message: _message,
    status: status ?? 500,
    isSuccess: false,
    isError: true,
    code,
  };
};

export const createRequestUrl = (path: string, queryParams?: TQueryParams) => {
  const _queryParams = queryParams ?? {};
  const obj = {
    url: queryString.stringifyUrl({
      url: process.env.API_SERVER + path,
      query: _queryParams,
    }),
    str: '',
  };

  if (queryParams) {
    obj.str = queryString.stringify(_queryParams);
  }

  return obj;
};

export const createRequest = async ({
  url,
  options,
}: {
  url: string;
  options?: Omit<RequestInit, 'body'> & {
    skipAuth?: boolean;
    skipBody?: boolean;
    skipHeader?: boolean;
    body?: any;
  };
}) => {
  const _options = options ?? {};
  let headers = _options.headers;
  let body = _options.body;

  if (!_options.skipAuth) {
    headers = {
      ...AUTHENTICATION_HEADER(),
    };
  }

  if (
    !_options.skipHeader &&
    (_options.method === 'POST' || _options.method === 'PUT')
  ) {
    headers = { ...headers, ...JSON_HEADER };
  }

  if (_options.headers) {
    headers = { ...headers, ..._options.headers };
  }

  if (_options.body && !_options.skipBody) {
    body = JSON.stringify(_options.body);
  }

  return fetch(url, {
    ...options,
    headers,
    body,
  });
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
        ? data.expDays * 24 * 60 * 60
        : undefined,
  });
};

const AUTHENTICATION_HEADER = (tk?: string): Record<string, string> => {
  const isHttpsSite = process.env.IS_HTTPS_SITE === 'true';
  const cookie = cookies().get(isHttpsSite ? SECURE_TK : TK);
  const value = cookie?.value ?? '';
  const _tk = tk ?? value;

  if (_tk) {
    return {
      [AUTHORIZATION]: `${BEARER} ${_tk}`,
    };
  }

  return {};
};

const TEST_AUTHENTICATION_HEADER = (tk = '') => {
  return {
    [AUTHORIZATION]: tk,
  };
};
