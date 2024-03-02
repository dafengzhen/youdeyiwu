import type {
  IErrorResponse,
  ISuccessResponse,
  TQueryParams,
} from '@/app/interfaces';
import queryString from 'query-string';
import { AUTHENTICATION_HEADER, JSON_HEADER } from '@/app/constants';

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
  if (typeof message === 'string') {
    _message = message;
  } else if (
    typeof message === 'object' &&
    typeof message.message === 'string'
  ) {
    _message = message.message;
  } else {
    _message = message ? message + '' : 'ERROR';
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
    body?: any;
  };
}) => {
  const _options = options ?? {};
  let headers: any = {};
  let body;

  if (!_options.skipAuth) {
    headers = {
      ...AUTHENTICATION_HEADER(),
    };
  }

  if (_options.method === 'POST' || _options.method === 'PUT') {
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
