'use server';

import { type IError } from '@/app/interfaces';
import { PUT } from '@/app/constants';
import { revalidateTag } from 'next/cache';
import {
  createErrorResponse,
  createRequest,
  createRequestUrl,
  createSuccessResponse,
} from '@/app/common/response';

export interface IUpdateRoleActionVariables {
  name: string;
  overview?: string;
  sort: number;
  display: boolean;
}

export default async function UpdateRoleAction({
  id,
  variables,
}: {
  id: number;
  variables: IUpdateRoleActionVariables;
}) {
  try {
    const { url } = createRequestUrl(`/roles/${id}`);
    const response = await createRequest({
      url,
      options: {
        method: PUT,
        body: variables,
        cache: 'no-store',
      },
    });

    if (!response.ok) {
      const data = (await response.json()) as IError;
      return createErrorResponse(data);
    }

    revalidateTag('/admin/roles');
    revalidateTag(`/admin/roles/${id}`);

    return createSuccessResponse(null);
  } catch (e) {
    return createErrorResponse(e);
  }
}
