'use server';

import type { IError, IPage, TQueryParams } from '@/app/[locale]/interfaces';
import type { ISectionGroup } from '@/app/[locale]/interfaces/section-groups';
import {
  createErrorResponse,
  createRequest,
  createRequestUrl,
  createSuccessResponse,
} from '@/app/[locale]/common/response';

export default async function QueryAllSectionGroupAction(
  queryParams?: TQueryParams,
) {
  try {
    const { url, str } = createRequestUrl('/section-groups', queryParams);
    const response = await createRequest({
      url,
      options: {
        next: {
          tags: ['/admin/section-groups', str],
        },
      },
    });

    const data = (await response.json()) as IPage<ISectionGroup[]> | IError;
    if (!response.ok) {
      return createErrorResponse(data);
    }

    return createSuccessResponse(data as IPage<ISectionGroup[]>);
  } catch (e) {
    return createErrorResponse(e);
  }
}
