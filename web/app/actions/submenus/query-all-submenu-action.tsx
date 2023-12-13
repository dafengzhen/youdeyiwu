'use server';

import { type IError } from '@/app/interfaces';
import FetchDataException from '@/app/exception/fetch-data-exception';
import { checkResponseStatus } from '@/app/common/server';
import { AUTHENTICATION_HEADER } from '@/app/constants';
import { ISubmenu } from '@/app/interfaces/menus';

export default async function QueryAllSubmenuAction() {
  const response = await fetch(process.env.API_SERVER + '/submenus', {
    headers: AUTHENTICATION_HEADER(),
    next: {
      tags: ['/admin/submenus'],
    },
  });

  const data = (await response.json()) as ISubmenu[] | IError;
  if (!response.ok) {
    checkResponseStatus(response.status);
    throw FetchDataException((data as IError).message);
  }

  return data as ISubmenu[];
}
