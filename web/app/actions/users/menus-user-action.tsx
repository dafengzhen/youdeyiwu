'use server';

import { type IError } from '@/app/interfaces';
import FetchDataException from '@/app/exception/fetch-data-exception';
import { AUTHENTICATION_HEADER } from '@/app/constants';
import { checkResponseStatus } from '@/app/common/server';
import { IMenu } from '@/app/interfaces/menus';

export default async function MenusUserAction() {
  const response = await fetch(process.env.API_SERVER + '/users/menus', {
    headers: AUTHENTICATION_HEADER(),
  });

  const data = (await response.json()) as IMenu[] | IError;
  if (!response.ok) {
    checkResponseStatus(response.status);
    throw FetchDataException((data as IError).message);
  }

  return data as IMenu[];
}
