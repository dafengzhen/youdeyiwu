'use server';

import { type IError } from '@/app/interfaces';
import FetchDataException from '@/app/exception/fetch-data-exception';
import { IPost } from '@/app/interfaces/posts';
import { AUTHENTICATION_HEADER } from '@/app/constants';
import { checkResponseStatus } from '@/app/common/server';

export default async function QueryRandomPostAction() {
  const response = await fetch(process.env.API_SERVER + '/posts/random', {
    headers: AUTHENTICATION_HEADER(),
    next: {
      tags: ['/admin/posts/random'],
    },
  });

  const data = (await response.json()) as IPost[] | IError;
  if (!response.ok) {
    checkResponseStatus(response.status);
    throw FetchDataException((data as IError).message);
  }

  return data as IPost[];
}
