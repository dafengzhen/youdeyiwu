'use server';

import { type IError } from '@/app/interfaces';
import { POST } from '@/app/constants';
import { revalidateTag } from 'next/cache';
import {
  createErrorResponse,
  createRequest,
  createRequestUrl,
  createSuccessResponse,
} from '@/app/common/response';

export interface ICreateRoleActionVariables {
  name: string;
  overview?: string;
  sort: number;
  display: boolean;
}

export default async function CreateRoleAction(
  variables: ICreateRoleActionVariables,
) {
  try {
    const { url, str } = createRequestUrl('/roles');
    const response = await createRequest({
      url,
      options: {
        method: POST,
        body: variables,
      },
    });

    if (!response.ok) {
      const data = (await response.json()) as IError;
      return createErrorResponse(data);
    }

    revalidateTag('/admin/roles');
    revalidateTag('/roles/select-all');

    return createSuccessResponse(null);
  } catch (e) {
    return createErrorResponse(e);
  }
}
