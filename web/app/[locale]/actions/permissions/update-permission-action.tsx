'use server';

import type { IError } from '@/app/[locale]/interfaces';
import { PUT } from '@/app/[locale]/constants';
import { revalidateTag } from 'next/cache';
import type {
  TPermissionMethod,
  TPermissionType,
} from '@/app/[locale]/interfaces/permissions';
import {
  createErrorResponse,
  createRequest,
  createRequestUrl,
  createSuccessResponse,
} from '@/app/[locale]/common/response';

export interface IUpdatePermissionActionVariables {
  name: string;
  alias?: string;
  overview?: string;
  method: TPermissionMethod;
  type: TPermissionType;
  sort: number;
  caseInsensitive: boolean;
  matchers?: number[];
}

export default async function UpdatePermissionAction({
  id,
  variables,
}: {
  id: number;
  variables: IUpdatePermissionActionVariables;
}) {
  try {
    const { url, str } = createRequestUrl(`/permissions/${id}`);
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
