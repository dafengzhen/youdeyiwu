'use server';

import { type IError } from '@/app/interfaces';
import FetchDataException from '@/app/exception/fetch-data-exception';
import { AUTHENTICATION_HEADER, JSON_HEADER, PUT } from '@/app/constants';
import { checkResponseStatus } from '@/app/common/server';

export interface IUpdatePointActionVariables {
  enable?: boolean;
  initPoints?: number;
}

export default async function UpdatePointConfigAction(
  variables: IUpdatePointActionVariables,
) {
  const response = await fetch(process.env.API_SERVER + '/configs/point', {
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
