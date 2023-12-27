'use server';

import { type IError } from '@/app/interfaces';
import FetchDataException from '@/app/exception/fetch-data-exception';
import { AUTHENTICATION_HEADER, JSON_HEADER, PUT } from '@/app/constants';
import { checkResponseStatus } from '@/app/common/server';
import { IReplyReviewState } from '@/app/interfaces/replies';

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
  const response = await fetch(
    process.env.API_SERVER + `/replies/${id}/state`,
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
