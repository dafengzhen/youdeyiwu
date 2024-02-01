'use server';

import { type IError } from '@/app/interfaces';
import FetchDataException from '@/app/exception/fetch-data-exception';
import { AUTHENTICATION_HEADER } from '@/app/constants';
import { checkResponseStatus } from '@/app/common/server';
import { IPointConfig } from '@/app/interfaces/configs';

export default async function QueryPointConfigAction() {
  const response = await fetch(process.env.API_SERVER + '/configs/point', {
    headers: AUTHENTICATION_HEADER(),
  });

  const data = (await response.json()) as IPointConfig | IError;
  if (!response.ok) {
    checkResponseStatus(response.status);
    throw FetchDataException((data as IError).message);
  }

  return data as IPointConfig;
}
