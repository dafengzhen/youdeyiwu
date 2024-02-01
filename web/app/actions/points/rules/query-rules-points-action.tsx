'use server';

import { type IError } from '@/app/interfaces';
import FetchDataException from '@/app/exception/fetch-data-exception';
import { checkResponseStatus } from '@/app/common/server';
import { AUTHENTICATION_HEADER } from '@/app/constants';
import { IPointRule } from '@/app/interfaces/points';

export default async function QueryRulesPointsAction() {
  const response = await fetch(process.env.API_SERVER + '/points/rules', {
    headers: AUTHENTICATION_HEADER(),
    next: {
      tags: ['/admin/points/rules'],
    },
  });

  const data = (await response.json()) as IPointRule[] | IError;
  if (!response.ok) {
    checkResponseStatus(response.status);
    throw FetchDataException((data as IError).message);
  }

  return data as IPointRule[];
}
