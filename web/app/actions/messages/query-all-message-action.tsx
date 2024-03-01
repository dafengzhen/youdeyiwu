'use server';

import { type IError, IPage, TQueryParams } from '@/app/interfaces';
import FetchDataException from '@/app/exception/fetch-data-exception';
import { checkResponseStatus } from '@/app/common/server';
import { AUTHENTICATION_HEADER } from '@/app/constants';
import { IMessage } from '@/app/interfaces/messages';
import queryString from 'query-string';

export default async function QueryAllMessageAction(
  queryParams?: TQueryParams,
) {
  const _queryParams = queryParams ?? {};
  const { url, str } = {
    url: queryString.stringifyUrl({
      url: process.env.API_SERVER + '/messages',
      query: _queryParams,
    }),
    str: queryString.stringify(_queryParams),
  };

  const response = await fetch(url, {
    headers: AUTHENTICATION_HEADER(),
    next: {
      tags: ['/admin/messages', str],
    },
  });

  const data = (await response.json()) as IPage<IMessage[]> | IError;
  if (!response.ok) {
    checkResponseStatus(response.status);
    throw FetchDataException((data as IError).message);
  }

  return data as IPage<IMessage[]>;
}
