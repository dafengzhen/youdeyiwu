'use server';

import { type IError, IPage, TQueryParams } from '@/app/interfaces';
import FetchDataException from '@/app/exception/fetch-data-exception';
import { checkResponseStatus } from '@/app/common/server';
import { IPost } from '@/app/interfaces/posts';
import { AUTHENTICATION_HEADER } from '@/app/constants';
import queryString from 'query-string';

export default async function QueryAllPostReviewQueuesAction(
  queryParams?: TQueryParams,
) {
  const _queryParams = queryParams ?? {};
  const { url, str } = {
    url: queryString.stringifyUrl({
      url: process.env.API_SERVER + '/posts/review-queues',
      query: _queryParams,
    }),
    str: queryString.stringify(_queryParams),
  };

  const response = await fetch(url, {
    headers: AUTHENTICATION_HEADER(),
    next: {
      tags: ['/admin/posts/review-queues', str],
    },
  });

  const data = (await response.json()) as IPage<IPost[]> | IError;
  if (!response.ok) {
    checkResponseStatus(response.status);
    throw FetchDataException((data as IError).message);
  }

  return data as IPage<IPost[]>;
}
