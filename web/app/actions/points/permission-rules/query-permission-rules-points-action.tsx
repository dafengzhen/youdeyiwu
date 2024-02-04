'use server';

import { type IError } from '@/app/interfaces';
import FetchDataException from '@/app/exception/fetch-data-exception';
import { checkResponseStatus } from '@/app/common/server';
import { AUTHENTICATION_HEADER } from '@/app/constants';
import { IPointPermissionRule } from '@/app/interfaces/points';

export default async function QueryPermissionRulesPointsAction() {
  const response = await fetch(
    process.env.API_SERVER + '/points/permission-rules',
    {
      headers: AUTHENTICATION_HEADER(),
      next: {
        tags: ['/admin/points/permission-rules'],
      },
    },
  );

  const data = (await response.json()) as IPointPermissionRule[] | IError;
  if (!response.ok) {
    checkResponseStatus(response.status);
    throw FetchDataException((data as IError).message);
  }

  return data as IPointPermissionRule[];
}
