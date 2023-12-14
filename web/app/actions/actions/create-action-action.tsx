'use server';

import { type IError } from '@/app/interfaces';
import FetchDataException from '@/app/exception/fetch-data-exception';
import { AUTHENTICATION_HEADER, JSON_HEADER, POST } from '@/app/constants';
import { revalidateTag } from 'next/cache';
import { checkResponseStatus } from '@/app/common/server';
import { TActionName } from '@/app/interfaces/menus';

export interface ICreateActionActionVariables {
  name?: TActionName;
  alias?: string;
  sort?: number;
}

export default async function CreateActionAction(
  variables: ICreateActionActionVariables,
) {
  const response = await fetch(process.env.API_SERVER + '/actions', {
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

  revalidateTag('/admin/actions');
}
