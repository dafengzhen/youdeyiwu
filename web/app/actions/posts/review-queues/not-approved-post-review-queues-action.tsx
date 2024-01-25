'use server';

import { type IError } from '@/app/interfaces';
import FetchDataException from '@/app/exception/fetch-data-exception';
import { AUTHENTICATION_HEADER, JSON_HEADER, POST } from '@/app/constants';
import { checkResponseStatus } from '@/app/common/server';

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
  const response = await fetch(
    process.env.API_SERVER + `/posts/review-queues/${id}/not-approved`,
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
