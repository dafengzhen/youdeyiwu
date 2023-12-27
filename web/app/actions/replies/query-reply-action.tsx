'use server';

import { type IError } from '@/app/interfaces';
import FetchDataException from '@/app/exception/fetch-data-exception';
import { AUTHENTICATION_HEADER } from '@/app/constants';
import { checkResponseStatus } from '@/app/common/server';
import { IReply } from '@/app/interfaces/replies';

export default async function QueryReplyAction(variables: {
  id: number | string;
}) {
  const response = await fetch(
    process.env.API_SERVER + `/replies/${variables.id}`,
    {
      headers: AUTHENTICATION_HEADER(),
      next: {
        tags: [`/admin/replies/${variables.id}`],
      },
    },
  );

  const data = (await response.json()) as IReply | IError;
  if (!response.ok) {
    checkResponseStatus(response.status);
    throw FetchDataException((data as IError).message);
  }

  return data as IReply;
}
