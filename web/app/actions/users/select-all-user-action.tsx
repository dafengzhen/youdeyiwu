'use server';

import { type IError, IPage, TQueryParams } from '@/app/interfaces';
import FetchDataException from '@/app/exception/fetch-data-exception';
import { AUTHENTICATION_HEADER } from '@/app/constants';
import { checkResponseStatus, getQueryParams } from '@/app/common/server';
import { IUser } from '@/app/interfaces/users';

export default async function SelectAllUserAction(queryParams?: TQueryParams) {
  let url = process.env.API_SERVER + '/users/select-all';
  if (queryParams) {
    url = url + '?' + getQueryParams(queryParams);
  }

  const response = await fetch(url, {
    headers: AUTHENTICATION_HEADER(),
    next: {
      tags: ['/users/select-all'],
    },
  });

  const data = (await response.json()) as IPage<IUser[]> | IError;
  if (!response.ok) {
    checkResponseStatus(response.status);
    throw FetchDataException((data as IError).message);
  }

  return data as IPage<IUser[]>;
}
