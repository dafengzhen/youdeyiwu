'use server';

import { type IError } from '@/app/interfaces';
import FetchDataException from '@/app/exception/fetch-data-exception';
import { checkResponseStatus } from '@/app/common/server';
import { AUTHENTICATION_HEADER } from '@/app/constants';
import { IPointAutoRule } from '@/app/interfaces/points';

export default async function QueryAutoRulesPointsAction() {
  const response = await fetch(process.env.API_SERVER + '/points/auto-rules', {
    headers: AUTHENTICATION_HEADER(),
    next: {
      tags: ['/admin/points/auto-rules'],
    },
  });

  const data = (await response.json()) as IPointAutoRule[] | IError;
  if (!response.ok) {
    checkResponseStatus(response.status);
    throw FetchDataException((data as IError).message);
  }

  return data as IPointAutoRule[];
}
