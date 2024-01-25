'use server';

import { type IError } from '@/app/interfaces';
import FetchDataException from '@/app/exception/fetch-data-exception';
import { AUTHENTICATION_HEADER, JSON_HEADER, POST } from '@/app/constants';
import { checkResponseStatus } from '@/app/common/server';

export interface IReceivePostReviewQueuesActionVariables {
  latestReviewResultTime: string;
  postId: number;
}

export default async function ReceivePostReviewQueuesAction(
  variables: IReceivePostReviewQueuesActionVariables,
) {
  const response = await fetch(
    process.env.API_SERVER + '/posts/review-queues/receive',
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
