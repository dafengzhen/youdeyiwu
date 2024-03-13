'use server';

import type { IError, IPage, TQueryParams } from '@/app/[locale]/interfaces';
import type { IRole } from '@/app/[locale]/interfaces/roles';
import {
  createErrorResponse,
  createRequest,
  createRequestUrl,
  createSuccessResponse,
} from '@/app/[locale]/common/response';

export default async function QueryAllRoleAction(queryParams?: TQueryParams) {
  try {
    const { url, str } = createRequestUrl('/roles', queryParams);
    const response = await createRequest({
      url,
      options: {
        next: {
          tags: ['/admin/roles', str],
        },
      },
    });

    const data = (await response.json()) as IPage<IRole[]> | IError;
    if (!response.ok) {
      return createErrorResponse(data);
    }

    return createSuccessResponse(data as IPage<IRole[]>);
  } catch (e) {
    return createErrorResponse(e);
  }
}
