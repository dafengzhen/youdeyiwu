'use server';

import { type IError } from '@/app/interfaces';
import FetchDataException from '@/app/exception/fetch-data-exception';
import { checkResponseStatus } from '@/app/common/server';
import { AUTHENTICATION_HEADER } from '@/app/constants';
import { IMenu } from '@/app/interfaces/menus';

export default async function QueryAllMenuAction() {
  const response = await fetch(process.env.API_SERVER + '/menus', {
    headers: AUTHENTICATION_HEADER(),
    next: {
      tags: ['/admin/menus'],
    },
  });

  const data = (await response.json()) as IMenu[] | IError;
  if (!response.ok) {
    checkResponseStatus(response.status);
    throw FetchDataException((data as IError).message);
  }

  return data as IMenu[];
}
