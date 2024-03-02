'use server';

import type { IError, IPage, TQueryParams } from '@/app/interfaces';
import type { IPost } from '@/app/interfaces/posts';
import {
  createErrorResponse,
  createRequest,
  createRequestUrl,
  createSuccessResponse,
} from '@/app/common/response';

export default async function QueryAllPostAction(queryParams?: TQueryParams) {
  try {
    const { url, str } = createRequestUrl('/posts');
    const response = await createRequest({
      url,
      options: {
        next: {
          tags: ['/admin/posts', str],
        },
      },
    });

    const data = (await response.json()) as IPage<IPost[]> | IError;
    if (!response.ok) {
      return createErrorResponse(data);
    }

    return createSuccessResponse(data as IPage<IPost[]>);
  } catch (e) {
    return createErrorResponse(e);
  }
}
