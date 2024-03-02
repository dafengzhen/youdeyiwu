'use server';

import type { IError, IPage, TQueryParams } from '@/app/interfaces';
import type { IPermission } from '@/app/interfaces/permissions';
import {
  createErrorResponse,
  createRequest,
  createRequestUrl,
  createSuccessResponse,
} from '@/app/common/response';

export default async function QueryAllPermissionAction(
  queryParams?: TQueryParams,
) {
  try {
    const { url, str } = createRequestUrl('/permissions', queryParams);
    const response = await createRequest({
      url,
      options: {
        next: {
          tags: ['/admin/permissions', str],
        },
      },
    });

    const data = (await response.json()) as IPage<IPermission[]> | IError;
    if (!response.ok) {
      return createErrorResponse(data);
    }

    return createSuccessResponse(data as IPage<IPermission[]>);
  } catch (e) {
    return createErrorResponse(e);
  }
}
