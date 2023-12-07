'use server';

import { type IError, IPage, TQueryParams } from '@/app/interfaces';
import FetchDataException from '@/app/exception/fetch-data-exception';
import { checkResponseStatus, getQueryParams } from '@/app/common/server';
import { IPost } from '@/app/interfaces/posts';
import { AUTHENTICATION_HEADER } from '@/app/constants';

export default async function QueryAllPostAction(queryParams?: TQueryParams) {
  let url = process.env.API_SERVER + '/posts';
  if (queryParams) {
    url = url + '?' + getQueryParams(queryParams);
  }

  const response = await fetch(url, {
    headers: AUTHENTICATION_HEADER(),
    next: {
      tags: ['/admin/posts'],
    },
  });

  const data = (await response.json()) as IPage<IPost[]> | IError;
  if (!response.ok) {
    checkResponseStatus(response.status);
    throw FetchDataException((data as IError).message);
  }

  return data as IPage<IPost[]>;
}
