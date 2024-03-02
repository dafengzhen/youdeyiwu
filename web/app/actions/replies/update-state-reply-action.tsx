'use server';

import type { IError } from '@/app/interfaces';
import { PUT } from '@/app/constants';
import type { IReplyReviewState } from '@/app/interfaces/replies';
import {
  createErrorResponse,
  createRequest,
  createRequestUrl,
  createSuccessResponse,
} from '@/app/common/response';

export interface IUpdateStateReplyActionVariables {
  reviewState: IReplyReviewState;
  reason?: string;
}

export default async function UpdateStateReplyAction({
  id,
  variables,
}: {
  id: number;
  variables: IUpdateStateReplyActionVariables;
}) {
  try {
    const { url, str } = createRequestUrl(`/replies/${id}/state`);
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
