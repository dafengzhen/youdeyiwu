'use server';

import { type IError } from '@/app/interfaces';
import FetchDataException from '@/app/exception/fetch-data-exception';
import { AUTHENTICATION_HEADER, JSON_HEADER, PUT } from '@/app/constants';
import { checkResponseStatus } from '@/app/common/server';

export interface IUpdateJwtActionVariables {
  secret?: string;
}

export default async function UpdateJwtConfigAction(
  variables: IUpdateJwtActionVariables,
) {
  const response = await fetch(process.env.API_SERVER + '/configs/jwt', {
    method: PUT,
    headers: {
      ...AUTHENTICATION_HEADER(),
      ...JSON_HEADER,
    },
    body: JSON.stringify(variables),
    cache: 'no-store',
  });

  if (!response.ok) {
    const data = (await response.json()) as IError;
    checkResponseStatus(response.status);
    throw FetchDataException(data.message);
  }
}
