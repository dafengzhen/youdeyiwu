'use server';

import { type IError } from '@/app/[locale]/interfaces';
import { PUT } from '@/app/[locale]/constants';
import { revalidateTag } from 'next/cache';
import {
  createErrorResponse,
  createRequest,
  createRequestUrl,
  createSuccessResponse,
} from '@/app/[locale]/common/response';

export interface IUpdateRolesPermissionActionVariables {
  roles?: number[];
}

export default async function UpdateRolesPermissionAction({
  id,
  variables,
}: {
  id: number;
  variables: IUpdateRolesPermissionActionVariables;
}) {
  try {
    const { url, str } = createRequestUrl(`/permissions/${id}/roles`);
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

    revalidateTag('/admin/permissions');
    revalidateTag(`/admin/permissions/${id}`);

    return createSuccessResponse(null);
  } catch (e) {
    return createErrorResponse(e);
  }
}
