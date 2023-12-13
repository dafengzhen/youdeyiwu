'use server';

import { type IError } from '@/app/interfaces';
import FetchDataException from '@/app/exception/fetch-data-exception';
import { AUTHENTICATION_HEADER } from '@/app/constants';
import { checkResponseStatus } from '@/app/common/server';
import { IJwtConfig } from '@/app/interfaces/configs';

export default async function QueryJwtConfigAction() {
  const response = await fetch(process.env.API_SERVER + '/configs/jwt', {
    headers: AUTHENTICATION_HEADER(),
  });

  const data = (await response.json()) as IJwtConfig | IError;
  if (!response.ok) {
    checkResponseStatus(response.status);
    throw FetchDataException((data as IError).message);
  }

  return data as IJwtConfig;
}
