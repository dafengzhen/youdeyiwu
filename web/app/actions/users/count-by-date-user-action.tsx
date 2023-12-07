'use server';

import { type IError } from '@/app/interfaces';
import FetchDataException from '@/app/exception/fetch-data-exception';
import { AUTHENTICATION_HEADER } from '@/app/constants';
import { TUsersCountByDate } from '@/app/interfaces/users';
import { checkResponseStatus } from '@/app/common/server';

export default async function CountByDateUserAction(variables?: {
  pastDays?: number;
}) {
  const response = await fetch(
    process.env.API_SERVER +
      `/users/count-by-date?pastDays=${variables?.pastDays ?? 15}`,
    {
      headers: AUTHENTICATION_HEADER(),
    },
  );

  const data = (await response.json()) as TUsersCountByDate | IError;
  if (!response.ok) {
    checkResponseStatus(response.status);
    throw FetchDataException((data as IError).message);
  }

  return data as TUsersCountByDate;
}
