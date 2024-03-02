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

export interface IUpdateRolesActionActionVariables {
  roles?: number[];
}

export default async function UpdateRolesActionAction({
  id,
  variables,
}: {
  id: number;
  variables: IUpdateRolesActionActionVariables;
}) {
  try {
    const { url } = createRequestUrl(`/actions/${id}/roles`);
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

    revalidateTag('/admin/actions');
    revalidateTag(`/admin/actions/${id}`);

    return createSuccessResponse(null);
  } catch (e) {
    return createErrorResponse(e);
  }
}
