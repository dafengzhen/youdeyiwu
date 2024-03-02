'use server';

import { type IError } from '@/app/interfaces';
import { PUT } from '@/app/constants';
import {
  createErrorResponse,
  createRequest,
  createRequestUrl,
  createSuccessResponse,
} from '@/app/common/response';

export interface IUpdateRootActionVariables {
  secret?: string;
}

export default async function UpdateRootConfigAction(
  variables: IUpdateRootActionVariables,
) {
  try {
    const { url } = createRequestUrl('/configs/root');
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
