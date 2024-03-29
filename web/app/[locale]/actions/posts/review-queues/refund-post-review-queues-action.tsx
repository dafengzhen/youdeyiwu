'use server';

import { type IError } from '@/app/[locale]/interfaces';
import { POST } from '@/app/[locale]/constants';
import {
  createErrorResponse,
  createRequest,
  createRequestUrl,
  createSuccessResponse,
} from '@/app/[locale]/common/response';

export interface IRefundPostReviewQueuesActionVariables {
  reason?: string;
}

export default async function RefundPostReviewQueuesAction({
  id,
  variables,
}: {
  id: number;
  variables: IRefundPostReviewQueuesActionVariables;
}) {
  try {
    const { url, str } = createRequestUrl(`/posts/review-queues/${id}/return`);
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
