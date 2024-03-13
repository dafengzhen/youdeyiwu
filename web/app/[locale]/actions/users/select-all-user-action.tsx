'use server';

import type { IError, IPage, TQueryParams } from '@/app/[locale]/interfaces';
import type { IUser } from '@/app/[locale]/interfaces/users';
import {
  createErrorResponse,
  createRequest,
  createRequestUrl,
  createSuccessResponse,
} from '@/app/[locale]/common/response';

export default async function SelectAllUserAction(queryParams?: TQueryParams) {
  try {
    const { url, str } = createRequestUrl('/users/select-all', queryParams);
    const response = await createRequest({
      url,
      options: {
        next: {
          tags: ['/users/select-all', str],
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
