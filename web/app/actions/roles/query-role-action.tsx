'use server';

import { type IError } from '@/app/interfaces';
import FetchDataException from '@/app/exception/fetch-data-exception';
import { AUTHENTICATION_HEADER } from '@/app/constants';
import { checkResponseStatus } from '@/app/common/server';
import { IRole } from '@/app/interfaces/roles';

export default async function QueryRoleAction(variables: {
  id: number | string;
}) {
  const response = await fetch(
    process.env.API_SERVER + `/roles/${variables.id}`,
    {
      headers: AUTHENTICATION_HEADER(),
      next: {
        tags: [`/admin/roles/${variables.id}`],
      },
    },
  );

  const data = (await response.json()) as IRole | IError;
  if (!response.ok) {
    checkResponseStatus(response.status);
    throw FetchDataException((data as IError).message);
  }

  return data as IRole;
}
