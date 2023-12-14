'use server';

import { type IError } from '@/app/interfaces';
import FetchDataException from '@/app/exception/fetch-data-exception';
import { AUTHENTICATION_HEADER, JSON_HEADER, PUT } from '@/app/constants';
import { revalidateTag } from 'next/cache';
import { checkResponseStatus } from '@/app/common/server';
import { TActionName } from '@/app/interfaces/menus';

export interface IUpdateActionActionVariables {
  name?: TActionName;
  alias?: string;
  sort?: number;
  menu?: number;
  submenu?: number;
}

export default async function UpdateActionAction({
  id,
  variables,
}: {
  id: number;
  variables: IUpdateActionActionVariables;
}) {
  const response = await fetch(process.env.API_SERVER + `/actions/${id}`, {
    method: PUT,
    headers: {
      ...AUTHENTICATION_HEADER(),
      ...JSON_HEADER,
    },
    body: JSON.stringify(variables),
    cache: 'no-store',
  });

  if (!response.ok) {
    const data = (await response.json()) as IError;
    checkResponseStatus(response.status);
    throw FetchDataException(data.message);
  }

  revalidateTag('/admin/actions');
  revalidateTag(`/admin/actions/${id}`);
}
