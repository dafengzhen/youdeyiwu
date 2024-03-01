'use server';

import { IError, IPage, TQueryParams } from '@/app/interfaces';
import { AUTHENTICATION_HEADER } from '@/app/constants';
import { IPost } from '@/app/interfaces/posts';
import { checkResponseStatus } from '@/app/common/server';
import FetchDataException from '@/app/exception/fetch-data-exception';
import queryString from 'query-string';

export default async function SelectAllPostAction(queryParams?: TQueryParams) {
  const _queryParams = queryParams ?? {};
  const { url, str } = {
    url: queryString.stringifyUrl({
      url: process.env.API_SERVER + '/posts/select-all',
      query: _queryParams,
    }),
    str: queryString.stringify(_queryParams),
  };

  const response = await fetch(url, {
    headers: AUTHENTICATION_HEADER(),
    next: {
      tags: ['/posts/select-all', str],
    },
  });

  const data = (await response.json()) as IPage<IPost[]> | IError;
  if (!response.ok) {
    checkResponseStatus(response.status);
    throw FetchDataException((data as IError).message);
  }

  return data as IPage<IPost[]>;
}
