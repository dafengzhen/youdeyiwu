'use server';

import { type IError } from '@/app/interfaces';
import FetchDataException from '@/app/exception/fetch-data-exception';
import { IPost } from '@/app/interfaces/posts';
import { AUTHENTICATION_HEADER } from '@/app/constants';
import { checkResponseStatus } from '@/app/common/server';

export default async function QueryPostAction(variables: {
  id: number | string;
}) {
  const response = await fetch(
    process.env.API_SERVER + `/posts/${variables.id}`,
    {
      headers: AUTHENTICATION_HEADER(),
      next: {
        tags: [`/admin/posts/${variables.id}`],
      },
    },
  );

  const data = (await response.json()) as IPost | IError;
  if (!response.ok) {
    checkResponseStatus(response.status);
    throw FetchDataException((data as IError).message);
  }

  return data as IPost;
}
