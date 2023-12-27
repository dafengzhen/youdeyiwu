'use server';

import { type IError } from '@/app/interfaces';
import FetchDataException from '@/app/exception/fetch-data-exception';
import { AUTHENTICATION_HEADER, JSON_HEADER, PUT } from '@/app/constants';
import { checkResponseStatus } from '@/app/common/server';
import { ICommentReviewState } from '@/app/interfaces/comments';

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
  const response = await fetch(
    process.env.API_SERVER + `/comments/${id}/state`,
    {
      method: PUT,
      headers: {
        ...AUTHENTICATION_HEADER(),
        ...JSON_HEADER,
      },
      body: JSON.stringify(variables),
      cache: 'no-store',
    },
  );

  if (!response.ok) {
    const data = (await response.json()) as IError;
    checkResponseStatus(response.status);
    throw FetchDataException(data.message);
  }
}
