'use server';

import { type IError } from '@/app/interfaces';
import FetchDataException from '@/app/exception/fetch-data-exception';
import { AUTHENTICATION_HEADER } from '@/app/constants';
import { checkResponseStatus } from '@/app/common/server';
import { IUser } from '@/app/interfaces/users';

export default async function LoginInfoUserAction() {
  const response = await fetch(process.env.API_SERVER + '/users/login-info', {
    headers: AUTHENTICATION_HEADER(),
  });

  if (!response.ok) {
    const data = (await response.json()) as IError;
    checkResponseStatus(response.status);
    throw FetchDataException(data.message);
  }

  const arrayBuffer = await response.arrayBuffer();
  return arrayBuffer.byteLength === 0
    ? null
    : (JSON.parse(new TextDecoder().decode(arrayBuffer)) as IUser);
}
