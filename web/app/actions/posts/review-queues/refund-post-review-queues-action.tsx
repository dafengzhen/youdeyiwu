'use server';

import { type IError } from '@/app/interfaces';
import FetchDataException from '@/app/exception/fetch-data-exception';
import { AUTHENTICATION_HEADER, JSON_HEADER, POST } from '@/app/constants';
import { checkResponseStatus } from '@/app/common/server';

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
  const response = await fetch(
    process.env.API_SERVER + `/posts/review-queues/${id}/return`,
    {
      method: POST,
      headers: {
        ...AUTHENTICATION_HEADER(),
        ...JSON_HEADER,
      },
      body: JSON.stringify(variables),
    },
  );

  if (!response.ok) {
    const data = (await response.json()) as IError;
    checkResponseStatus(response.status);
    throw FetchDataException(data.message);
  }
}
