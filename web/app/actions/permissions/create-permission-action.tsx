'use server';

import { type IError } from '@/app/interfaces';
import FetchDataException from '@/app/exception/fetch-data-exception';
import { AUTHENTICATION_HEADER, JSON_HEADER, POST } from '@/app/constants';
import { revalidateTag } from 'next/cache';
import { checkResponseStatus } from '@/app/common/server';
import {
  TPermissionMethod,
  TPermissionType,
} from '@/app/interfaces/permissions';

export interface ICreatePermissionActionVariables {
  name: string;
  alias?: string;
  overview?: string;
  method: TPermissionMethod;
  type: TPermissionType;
  sort: number;
  caseInsensitive: boolean;
  matchers?: number[];
}

export default async function CreatePermissionAction(
  variables: ICreatePermissionActionVariables,
) {
  const response = await fetch(process.env.API_SERVER + '/permissions', {
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

  revalidateTag('/admin/permissions');
  revalidateTag('/permissions/select-all');
}
