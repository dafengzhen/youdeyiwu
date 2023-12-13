'use server';

import { type IError } from '@/app/interfaces';
import FetchDataException from '@/app/exception/fetch-data-exception';
import { checkResponseStatus } from '@/app/common/server';
import { AUTHENTICATION_HEADER } from '@/app/constants';
import { IAction } from '@/app/interfaces/menus';

export default async function QueryAllActionAction() {
  const response = await fetch(process.env.API_SERVER + '/actions', {
    headers: AUTHENTICATION_HEADER(),
    next: {
      tags: ['/admin/actions'],
    },
  });

  const data = (await response.json()) as IAction[] | IError;
  if (!response.ok) {
    checkResponseStatus(response.status);
    throw FetchDataException((data as IError).message);
  }

  return data as IAction[];
}
