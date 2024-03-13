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

export interface IUpdateRolesSubmenuActionVariables {
  roles?: number[];
}

export default async function UpdateRolesSubmenuAction({
  id,
  variables,
}: {
  id: number;
  variables: IUpdateRolesSubmenuActionVariables;
}) {
  try {
    const { url } = createRequestUrl(`/menus/${id}/roles`);
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

    revalidateTag('/admin/menus');
    revalidateTag(`/admin/menus/${id}`);

    return createSuccessResponse(null);
  } catch (e) {
    return createErrorResponse(e);
  }
}
