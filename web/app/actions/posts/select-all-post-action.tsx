'use server';

import { type IError, IPage, TQueryParams } from '@/app/interfaces';
import FetchDataException from '@/app/exception/fetch-data-exception';
import { IPost } from '@/app/interfaces/posts';
import { checkResponseStatus, getQueryParams } from '@/app/common/server';
import { AUTHENTICATION_HEADER } from '@/app/constants';

export default async function SelectAllPostAction(queryParams?: TQueryParams) {
  let url = process.env.API_SERVER + '/posts/select-all';
  let params = '';
  if (queryParams) {
    params = getQueryParams(queryParams);
    url = url + '?' + params;
  }

  const response = await fetch(url, {
    headers: AUTHENTICATION_HEADER(),
    next: {
      tags: ['/posts/select-all' + params ? '?' + params : ''],
    },
  });

  const data = (await response.json()) as IPage<IPost[]> | IError;
  if (!response.ok) {
    checkResponseStatus(response.status);
    throw FetchDataException((data as IError).message);
  }

  return data as IPage<IPost[]>;
}
