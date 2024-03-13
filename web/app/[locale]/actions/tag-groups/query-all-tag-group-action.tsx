'use server';

import type { IError, IPage, TQueryParams } from '@/app/[locale]/interfaces';
import type { ITagGroup } from '@/app/[locale]/interfaces/tag-groups';
import {
  createErrorResponse,
  createRequest,
  createRequestUrl,
  createSuccessResponse,
} from '@/app/[locale]/common/response';

export default async function QueryAllTagGroupAction(
  queryParams?: TQueryParams,
) {
  try {
    const { url, str } = createRequestUrl('/tag-groups', queryParams);
    const response = await createRequest({
      url,
      options: {
        next: {
          tags: ['/admin/tag-groups', str],
        },
      },
    });

    const data = (await response.json()) as IPage<ITagGroup[]> | IError;
    if (!response.ok) {
      return createErrorResponse(data);
    }

    return createSuccessResponse(data as IPage<ITagGroup[]>);
  } catch (e) {
    return createErrorResponse(e);
  }
}
