'use server';

import type { IError, IPage, TQueryParams } from '@/app/interfaces';
import type { IUser } from '@/app/interfaces/users';
import {
  createErrorResponse,
  createRequest,
  createRequestUrl,
  createSuccessResponse,
} from '@/app/common/response';

export default async function QueryAllUserAction(queryParams?: TQueryParams) {
  try {
    const { url, str } = createRequestUrl('/users', queryParams);
    const response = await createRequest({
      url,
      options: {
        next: {
          tags: ['/admin/users', str],
        },
      },
    });

    const data = (await response.json()) as IPage<IUser[]> | IError;
    if (!response.ok) {
      return createErrorResponse(data);
    }

    return createSuccessResponse(data as IPage<IUser[]>);
  } catch (e) {
    return createErrorResponse(e);
  }
}
