'use server';

import { type IError, IPage, TQueryParams } from '@/app/interfaces';
import FetchDataException from '@/app/exception/fetch-data-exception';
import type { ISection } from '@/app/interfaces/sections';
import { checkResponseStatus, getQueryParams } from '@/app/common/server';
import { AUTHENTICATION_HEADER } from '@/app/constants';

export default async function QueryAllSectionAction(
  queryParams?: TQueryParams,
) {
  let url = process.env.API_SERVER + '/sections';
  if (queryParams) {
    url = url + '?' + getQueryParams(queryParams);
  }

  const response = await fetch(url, {
    headers: AUTHENTICATION_HEADER(),
    next: {
      tags: ['/admin/sections'],
    },
  });

  const data = (await response.json()) as IPage<ISection[]> | IError;
  if (!response.ok) {
    checkResponseStatus(response.status);
    throw FetchDataException((data as IError).message);
  }

  return data as IPage<ISection[]>;
}
