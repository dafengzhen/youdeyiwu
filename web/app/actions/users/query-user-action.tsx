'use server';

import { type IError } from '@/app/interfaces';
import FetchDataException from '@/app/exception/fetch-data-exception';
import { AUTHENTICATION_HEADER } from '@/app/constants';
import { checkResponseStatus } from '@/app/common/server';
import { IUser } from '@/app/interfaces/users';

export default async function QueryUserAction(variables: {
  id: number | string;
}) {
  const response = await fetch(
    process.env.API_SERVER + `/users/${variables.id}`,
    {
      headers: AUTHENTICATION_HEADER(),
      next: {
        tags: [`/admin/users/${variables.id}`],
      },
    },
  );

  const data = (await response.json()) as IUser | IError;
  if (!response.ok) {
    checkResponseStatus(response.status);
    throw FetchDataException((data as IError).message);
  }

  return data as IUser;
}
