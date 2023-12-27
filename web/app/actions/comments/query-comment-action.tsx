'use server';

import { type IError } from '@/app/interfaces';
import FetchDataException from '@/app/exception/fetch-data-exception';
import { AUTHENTICATION_HEADER } from '@/app/constants';
import { checkResponseStatus } from '@/app/common/server';
import { IComment } from '@/app/interfaces/comments';

export default async function QueryCommentAction(variables: {
  id: number | string;
}) {
  const response = await fetch(
    process.env.API_SERVER + `/comments/${variables.id}`,
    {
      headers: AUTHENTICATION_HEADER(),
      next: {
        tags: [`/admin/comments/${variables.id}`],
      },
    },
  );

  const data = (await response.json()) as IComment | IError;
  if (!response.ok) {
    checkResponseStatus(response.status);
    throw FetchDataException((data as IError).message);
  }

  return data as IComment;
}
