'use server';

import { type IError } from '@/app/interfaces';
import FetchDataException from '@/app/exception/fetch-data-exception';
import { IPostDetails } from '@/app/interfaces/posts';
import { AUTHENTICATION_HEADER } from '@/app/constants';
import { checkResponseStatus } from '@/app/common/server';

export default async function QueryDetailsPostAction(variables: {
  id: number | string;
}) {
  const response = await fetch(
    process.env.API_SERVER + `/posts/${variables.id}/details`,
    {
      headers: AUTHENTICATION_HEADER(),
      next: {
        tags: [`/admin/posts/${variables.id}/details`],
      },
    },
  );

  const data = (await response.json()) as IPostDetails | IError;
  if (!response.ok) {
    checkResponseStatus(response.status);
    throw FetchDataException((data as IError).message);
  }

  return data as IPostDetails;
}
