'use server';

import type { IError, IPage, TQueryParams } from '@/app/[locale]/interfaces';
import type { IPostUser } from '@/app/[locale]/interfaces/posts';
import {
  createErrorResponse,
  createRequest,
  createRequestUrl,
  createSuccessResponse,
} from '@/app/[locale]/common/response';

export default async function QueryUserRelationshipPostAction(variables: {
  id: number;
  queryParams?: TQueryParams;
}) {
  try {
    const path = `/posts/${variables.id}/user-relationship`;
    const { url, str } = createRequestUrl(path, variables.queryParams);
    const response = await createRequest({
      url,
      options: {
        next: {
          tags: [path, str],
        },
      },
    });

    const data = (await response.json()) as IPage<IPostUser[]> | IError;
    if (!response.ok) {
      return createErrorResponse(data);
    }

    return createSuccessResponse(data as IPage<IPostUser[]>);
  } catch (e) {
    return createErrorResponse(e);
  }
}
