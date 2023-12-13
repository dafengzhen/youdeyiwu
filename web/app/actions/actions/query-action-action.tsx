'use server';

import { type IError } from '@/app/interfaces';
import FetchDataException from '@/app/exception/fetch-data-exception';
import { AUTHENTICATION_HEADER } from '@/app/constants';
import { checkResponseStatus } from '@/app/common/server';
import { IAction } from '@/app/interfaces/menus';

export default async function QueryActionAction(variables: {
  id: number | string;
}) {
  const response = await fetch(
    process.env.API_SERVER + `/actions/${variables.id}`,
    {
      headers: AUTHENTICATION_HEADER(),
      next: {
        tags: [`/admin/actions/${variables.id}`],
      },
    },
  );

  const data = (await response.json()) as IAction | IError;
  if (!response.ok) {
    checkResponseStatus(response.status);
    throw FetchDataException((data as IError).message);
  }

  return data as IAction;
}
