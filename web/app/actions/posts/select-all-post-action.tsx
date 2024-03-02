'use server';

import type { IError, IPage, TQueryParams } from '@/app/interfaces';
import type { IPost } from '@/app/interfaces/posts';
import {
  createErrorResponse,
  createRequest,
  createRequestUrl,
  createSuccessResponse,
} from '@/app/common/response';

export default async function SelectAllPostAction(queryParams?: TQueryParams) {
  try {
    const { url, str } = createRequestUrl('/posts/select-all', queryParams);
    const response = await createRequest({
      url,
      options: {
        next: {
          tags: ['/posts/select-all', str],
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
