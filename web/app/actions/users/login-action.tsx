'use server';

import type { IError, IToken } from '@/app/interfaces';
import { POST } from '@/app/constants';
import HealthAction from '@/app/actions/health-action';
import {
  createErrorResponse,
  createRequest,
  createRequestUrl,
  createSuccessResponse,
  setCredentials,
} from '@/app/common/response';

export interface ILoginActionVariables {
  username: string;
  password: string;
}

export default async function LoginAction(variables: ILoginActionVariables) {
  try {
    const requestPreCheck = (await HealthAction()).isSuccess;
    const { url, str } = createRequestUrl('/users/login');
    const response = await createRequest({
      url,
      options: {
        method: POST,
        body: variables,
        skipAuth: !requestPreCheck,
      },
    });

    const data = (await response.json()) as IToken | IError;
    if (!response.ok) {
      return createErrorResponse(data);
    }

    setCredentials(data as IToken);
    return createSuccessResponse(null);
  } catch (e) {
    return createErrorResponse(e);
  }
}
