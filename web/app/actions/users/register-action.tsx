'use server';

import type { IError, IToken } from '@/app/interfaces';
import { POST } from '@/app/constants';
import { setCredentials } from '@/app/common/server';
import HealthAction from '@/app/actions/health-action';
import { revalidateTag } from 'next/cache';
import {
  createErrorResponse,
  createRequest,
  createRequestUrl,
  createSuccessResponse,
} from '@/app/common/response';

export interface IRegisterActionVariables {
  username: string;
  password: string;
}

export default async function RegisterAction(
  variables: IRegisterActionVariables,
) {
  try {
    const requestPreCheck = (await HealthAction()).isSuccess;
    const { url, str } = createRequestUrl('/users/register');
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
    revalidateTag('/admin/users');
    return createSuccessResponse(null);
  } catch (e) {
    return createErrorResponse(e);
  }
}
