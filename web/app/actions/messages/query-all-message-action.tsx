'use server';

import { type IError, IPage, TQueryParams } from '@/app/interfaces';
import FetchDataException from '@/app/exception/fetch-data-exception';
import { checkResponseStatus, getQueryParams } from '@/app/common/server';
import { AUTHENTICATION_HEADER } from '@/app/constants';
import { IMessage } from '@/app/interfaces/messages';

export default async function QueryAllMessageAction(
  queryParams?: TQueryParams,
) {
  let url = process.env.API_SERVER + '/messages';
  if (queryParams) {
    url = url + '?' + getQueryParams(queryParams);
  }

  const response = await fetch(url, {
    headers: AUTHENTICATION_HEADER(),
    next: {
      tags: ['/admin/messages'],
    },
  });

  const data = (await response.json()) as IPage<IMessage[]> | IError;
  if (!response.ok) {
    checkResponseStatus(response.status);
    throw FetchDataException((data as IError).message);
  }

  return data as IPage<IMessage[]>;
}
