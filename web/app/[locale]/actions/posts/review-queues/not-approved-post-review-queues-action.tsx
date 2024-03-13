'use server';

import { type IError } from '@/app/[locale]/interfaces';
import { POST } from '@/app/[locale]/constants';
import {
  createErrorResponse,
  createRequest,
  createRequestUrl,
  createSuccessResponse,
} from '@/app/[locale]/common/response';

export interface INotApprovedPostReviewQueuesActionVariables {
  refundReason?: string;
}

export default async function NotApprovedPostReviewQueuesAction({
  id,
  variables,
}: {
  id: number;
  variables: INotApprovedPostReviewQueuesActionVariables;
}) {
  try {
    const { url } = createRequestUrl(`/posts/review-queues/${id}/not-approved`);
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
