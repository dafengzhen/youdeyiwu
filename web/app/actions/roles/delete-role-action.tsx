'use server';

import { type IError } from '@/app/interfaces';
import FetchDataException from '@/app/exception/fetch-data-exception';
import { AUTHENTICATION_HEADER, DELETE } from '@/app/constants';
import { checkResponseStatus } from '@/app/common/server';

export default async function DeleteRoleAction(variables: { id: number }) {
  const response = await fetch(
    process.env.API_SERVER + `/roles/${variables.id}`,
    {
      headers: AUTHENTICATION_HEADER(),
      method: DELETE,
    },
  );

  if (!response.ok) {
    const data = (await response.json()) as IError;
    checkResponseStatus(response.status);
    throw FetchDataException(data.message);
  }
}
