'use server';

import { type IError } from '@/app/[locale]/interfaces';
import { POST } from '@/app/[locale]/constants';
import {
  createErrorResponse,
  createRequest,
  createRequestUrl,
  createSuccessResponse,
} from '@/app/[locale]/common/response';

export interface IReceivePostReviewQueuesActionVariables {
  latestReviewResultTime: string;
  postId: number;
}

export default async function ReceivePostReviewQueuesAction(
  variables: IReceivePostReviewQueuesActionVariables,
) {
  try {
    const { url, str } = createRequestUrl('/posts/review-queues/receive');
    const response = await createRequest({
      url,
      options: {
        method: POST,
        body: variables,
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
