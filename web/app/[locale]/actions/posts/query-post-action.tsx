'use server';

import type { IError } from '@/app/[locale]/interfaces';
import type { IPost } from '@/app/[locale]/interfaces/posts';
import {
  createErrorResponse,
  createRequest,
  createRequestUrl,
  createSuccessResponse,
} from '@/app/[locale]/common/response';

export default async function QueryPostAction(variables: {
  id: number | string;
}) {
  try {
    const { url } = createRequestUrl(`/posts/${variables.id}`);
    const response = await createRequest({
      url,
      options: {
        next: {
          tags: [`/admin/posts/${variables.id}`],
        },
      },
    });

    const data = (await response.json()) as IPost | IError;
    if (!response.ok) {
      return createErrorResponse(data);
    }

    return createSuccessResponse(data as IPost);
  } catch (e) {
    return createErrorResponse(e);
  }
}
