'use server';

import type { IError, IPage, TQueryParams } from '@/app/interfaces';
import type { ISection } from '@/app/interfaces/sections';
import {
  createErrorResponse,
  createRequest,
  createRequestUrl,
  createSuccessResponse,
} from '@/app/common/response';

export default async function QueryAllSectionAction(
  queryParams?: TQueryParams,
) {
  try {
    const { url, str } = createRequestUrl('/sections', queryParams);
    const response = await createRequest({
      url,
      options: {
        next: {
          tags: ['/admin/sections', str],
        },
      },
    });

    const data = (await response.json()) as IPage<ISection[]> | IError;
    if (!response.ok) {
      return createErrorResponse(data);
    }

    return createSuccessResponse(data as IPage<ISection[]>);
  } catch (e) {
    return createErrorResponse(e);
  }
}
