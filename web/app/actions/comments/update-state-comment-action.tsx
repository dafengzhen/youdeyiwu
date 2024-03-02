'use server';

import type { IError } from '@/app/interfaces';
import { PUT } from '@/app/constants';
import type { ICommentReviewState } from '@/app/interfaces/comments';
import {
  createErrorResponse,
  createRequest,
  createRequestUrl,
  createSuccessResponse,
} from '@/app/common/response';

export interface IUpdateStateCommentActionVariables {
  reviewState: ICommentReviewState;
  reason?: string;
}

export default async function UpdateStateCommentAction({
  id,
  variables,
}: {
  id: number;
  variables: IUpdateStateCommentActionVariables;
}) {
  try {
    const { url } = createRequestUrl(`/comments/${id}/state`);
    const response = await createRequest({
      url,
      options: {
        method: PUT,
        body: variables,
        cache: 'no-store',
      },
    });

    if (!response.ok) {
      const data = (await response.json()) as IError;
      return createErrorResponse(data);
    }

    return createSuccessResponse(null);
  } catch (e) {
    return createErrorResponse(e);
  }
}
