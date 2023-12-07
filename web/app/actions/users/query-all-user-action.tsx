'use server';

import { type IError, IPage, TQueryParams } from '@/app/interfaces';
import FetchDataException from '@/app/exception/fetch-data-exception';
import { checkResponseStatus, getQueryParams } from '@/app/common/server';
import { AUTHENTICATION_HEADER } from '@/app/constants';
import { IUser } from '@/app/interfaces/users';

export default async function QueryAllUserAction(queryParams?: TQueryParams) {
  let url = process.env.API_SERVER + '/users';
  if (queryParams) {
    url = url + '?' + getQueryParams(queryParams);
  }

  const response = await fetch(url, {
    headers: AUTHENTICATION_HEADER(),
    next: {
      tags: ['/admin/users'],
    },
  });

  const data = (await response.json()) as IPage<IUser[]> | IError;
  if (!response.ok) {
    checkResponseStatus(response.status);
    throw FetchDataException((data as IError).message);
  }

  return data as IPage<IUser[]>;
}
