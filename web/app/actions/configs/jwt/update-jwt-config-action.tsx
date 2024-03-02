'use server';

import { type IError } from '@/app/interfaces';
import { PUT } from '@/app/constants';
import {
  createErrorResponse,
  createRequest,
  createRequestUrl,
  createSuccessResponse,
} from '@/app/common/response';

export interface IUpdateJwtActionVariables {
  secret?: string;
}

export default async function UpdateJwtConfigAction(
  variables: IUpdateJwtActionVariables,
) {
  try {
    const { url } = createRequestUrl('/configs/jwt');
    const response = await createRequest({
      url,
      options: {
        method: PUT,
        body: variables,
        cache: 'no-store',
      },
    });

    if (!response.ok) {
      const data = (await response.json()) as IError;
      return createErrorResponse(data);
    }

    return createSuccessResponse(null);
  } catch (e) {
    return createErrorResponse(e);
  }
}
