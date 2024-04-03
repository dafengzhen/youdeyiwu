'use server';

import { type IError } from '@/app/[locale]/interfaces';
import { PUT } from '@/app/[locale]/constants';
import {
  createErrorResponse,
  createRequest,
  createRequestUrl,
  createSuccessResponse,
} from '@/app/[locale]/common/response';

export interface IUpdateTemporaryStorageUserActionVariables {
  temporaryStorage?: string;
}

export default async function UpdateTemporaryStorageUserAction(
  variables: IUpdateTemporaryStorageUserActionVariables,
) {
  try {
    const { url } = createRequestUrl('/users/temporary-storage');
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
