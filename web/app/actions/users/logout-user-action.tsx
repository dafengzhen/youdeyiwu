'use server';

import { type IError } from '@/app/interfaces';
import FetchDataException from '@/app/exception/fetch-data-exception';
import { AUTHENTICATION_HEADER, JSON_HEADER, POST } from '@/app/constants';
import { revalidateTag } from 'next/cache';
import { checkResponseStatus, deleteTicket } from '@/app/common/server';

export default async function LogoutUserAction(variables: {
  id: number | string;
}) {
  const response = await fetch(process.env.API_SERVER + '/users/logout', {
    method: POST,
    headers: {
      ...AUTHENTICATION_HEADER(),
      ...JSON_HEADER,
    },
    cache: 'no-store',
  });

  if (!response.ok) {
    const data = (await response.json()) as IError;
    checkResponseStatus(response.status);
    throw FetchDataException(data.message);
  }

  revalidateTag('/admin/users');
  revalidateTag(`/admin/users/${variables.id}`);
  deleteTicket();
}
