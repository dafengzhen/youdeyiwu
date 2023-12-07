'use server';

import { type IError, IPage, TQueryParams } from '@/app/interfaces';
import FetchDataException from '@/app/exception/fetch-data-exception';
import { checkResponseStatus, getQueryParams } from '@/app/common/server';
import { ITag } from '@/app/interfaces/tags';
import { AUTHENTICATION_HEADER } from '@/app/constants';

export default async function QueryAllTagAction(queryParams?: TQueryParams) {
  let url = process.env.API_SERVER + '/tags';
  if (queryParams) {
    url = url + '?' + getQueryParams(queryParams);
  }

  const response = await fetch(url, {
    headers: AUTHENTICATION_HEADER(),
    next: {
      tags: ['/admin/tags'],
    },
  });

  const data = (await response.json()) as IPage<ITag[]> | IError;
  if (!response.ok) {
    checkResponseStatus(response.status);
    throw FetchDataException((data as IError).message);
  }

  return data as IPage<ITag[]>;
}
