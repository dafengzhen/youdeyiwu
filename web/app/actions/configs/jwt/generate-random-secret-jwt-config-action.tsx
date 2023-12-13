'use server';

import { type IError } from '@/app/interfaces';
import FetchDataException from '@/app/exception/fetch-data-exception';
import { AUTHENTICATION_HEADER } from '@/app/constants';
import { checkResponseStatus } from '@/app/common/server';

export default async function GenerateRandomSecretJwtConfigAction() {
  const response = await fetch(
    process.env.API_SERVER + '/configs/jwt/generate-random-secret',
    {
      headers: AUTHENTICATION_HEADER(),
    },
  );

  if (!response.ok) {
    const data = (await response.json()) as IError;
    checkResponseStatus(response.status);
    throw FetchDataException(data.message);
  }

  return await response.text();
}
