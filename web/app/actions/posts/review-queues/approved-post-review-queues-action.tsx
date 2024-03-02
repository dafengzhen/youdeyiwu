'use server';

import { type IError } from '@/app/interfaces';
import { POST } from '@/app/constants';
import {
  createErrorResponse,
  createRequest,
  createRequestUrl,
  createSuccessResponse,
} from '@/app/common/response';

export interface IApprovedPostReviewQueuesActionVariables {
  refundReason?: string;
}

export default async function ApprovedPostReviewQueuesAction({
  id,
  variables,
}: {
  id: number;
  variables: IApprovedPostReviewQueuesActionVariables;
}) {
  try {
    const { url } = createRequestUrl(`/posts/review-queues/${id}/approved`);
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
