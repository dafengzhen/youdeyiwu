'use server';

import type { IError } from '@/app/interfaces';
import type { IComment } from '@/app/interfaces/comments';
import {
  createErrorResponse,
  createRequest,
  createRequestUrl,
  createSuccessResponse,
} from '@/app/common/response';

export default async function QueryCommentAction(variables: {
  id: number | string;
}) {
  try {
    const { url } = createRequestUrl(`/comments/${variables.id}`);
    const response = await createRequest({
      url,
      options: {
        next: {
          tags: [`/admin/comments/${variables.id}`],
        },
      },
    });

    const data = (await response.json()) as IComment | IError;
    if (!response.ok) {
      return createErrorResponse(data);
    }

    return createSuccessResponse(data as IComment);
  } catch (e) {
    return createErrorResponse(e);
  }
}
