'use server';

import { type IError } from '@/app/interfaces';
import FetchDataException from '@/app/exception/fetch-data-exception';
import { AUTHENTICATION_HEADER, JSON_HEADER, POST } from '@/app/constants';
import { revalidateTag } from 'next/cache';
import { checkResponseStatus } from '@/app/common/server';

export interface ICreateRoleActionVariables {
  name: string;
  overview?: string;
  sort: number;
  display: boolean;
}

export default async function CreateRoleAction(
  variables: ICreateRoleActionVariables,
) {
  const response = await fetch(process.env.API_SERVER + '/roles', {
    method: POST,
    headers: {
      ...AUTHENTICATION_HEADER(),
      ...JSON_HEADER,
    },
    body: JSON.stringify(variables),
  });

  if (!response.ok) {
    const data = (await response.json()) as IError;
    checkResponseStatus(response.status);
    throw FetchDataException(data.message);
  }

  revalidateTag('/admin/roles');
  revalidateTag('/roles/select-all');
}
