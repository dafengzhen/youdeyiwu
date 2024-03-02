'use server';

import type { IError, IPage, TQueryParams } from '@/app/interfaces';
import type { ITag } from '@/app/interfaces/tags';
import {
  createErrorResponse,
  createRequest,
  createRequestUrl,
  createSuccessResponse,
} from '@/app/common/response';

export default async function QueryAllTagAction(queryParams?: TQueryParams) {
  try {
    const { url, str } = createRequestUrl('/tags', queryParams);
    const response = await createRequest({
      url,
      options: {
        next: {
          tags: ['/admin/tags', str],
        },
      },
    });

    const data = (await response.json()) as IPage<ITag[]> | IError;
    if (!response.ok) {
      return createErrorResponse(data);
    }

    return createSuccessResponse(data as IPage<ITag[]>);
  } catch (e) {
    return createErrorResponse(e);
  }
}
