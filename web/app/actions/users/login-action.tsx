'use server';

import { type IError, IToken } from '@/app/interfaces';
import FetchDataException from '@/app/exception/fetch-data-exception';
import { AUTHENTICATION_HEADER, JSON_HEADER, POST } from '@/app/constants';
import { setCredentials } from '@/app/common/server';
import HealthAction from '@/app/actions/health-action';

export interface ILoginActionVariables {
  username: string;
  password: string;
}

export default async function LoginAction(variables: ILoginActionVariables) {
  let requestPreCheck: boolean;
  try {
    await HealthAction();
    requestPreCheck = true;
  } catch (e) {
    requestPreCheck = false;
  }

  const response = await fetch(process.env.API_SERVER + '/users/login', {
    method: POST,
    headers: {
      ...(requestPreCheck ? AUTHENTICATION_HEADER() : {}),
      ...JSON_HEADER,
    },
    body: JSON.stringify(variables),
  });

  const data = (await response.json()) as IToken | IError;
  if (!response.ok) {
    throw FetchDataException((data as IError).message);
  }

  setCredentials(data as IToken);
}
