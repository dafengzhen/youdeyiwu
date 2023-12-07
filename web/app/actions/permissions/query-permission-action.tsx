'use server';

import { type IError } from '@/app/interfaces';
import FetchDataException from '@/app/exception/fetch-data-exception';
import { AUTHENTICATION_HEADER } from '@/app/constants';
import { checkResponseStatus } from '@/app/common/server';
import { IPermission } from '@/app/interfaces/permissions';

export default async function QueryPermissionAction(variables: {
  id: number | string;
}) {
  const response = await fetch(
    process.env.API_SERVER + `/permissions/${variables.id}`,
    {
      headers: AUTHENTICATION_HEADER(),
      next: {
        tags: [`/admin/permissions/${variables.id}`],
      },
    },
  );

  const data = (await response.json()) as IPermission | IError;
  if (!response.ok) {
    checkResponseStatus(response.status);
    throw FetchDataException((data as IError).message);
  }

  return data as IPermission;
}
