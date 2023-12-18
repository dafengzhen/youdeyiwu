'use server';

import { type IError, IPage, TQueryParams } from '@/app/interfaces';
import FetchDataException from '@/app/exception/fetch-data-exception';
import { checkResponseStatus, getQueryParams } from '@/app/common/server';
import { AUTHENTICATION_HEADER } from '@/app/constants';
import { IGlobalMessage } from '@/app/interfaces/messages';

export default async function QueryAllGlobalMessagesAction(
  queryParams?: TQueryParams,
) {
  let url = process.env.API_SERVER + '/messages/global-messages';
  if (queryParams) {
    url = url + '?' + getQueryParams(queryParams);
  }

  const response = await fetch(url, {
    headers: AUTHENTICATION_HEADER(),
    next: {
      tags: ['/admin/messages/global-messages'],
    },
  });

  const data = (await response.json()) as IPage<IGlobalMessage[]> | IError;
  if (!response.ok) {
    checkResponseStatus(response.status);
    throw FetchDataException((data as IError).message);
  }

  return data as IPage<IGlobalMessage[]>;
}
