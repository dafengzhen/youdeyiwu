'use server';

import type { IError } from '@/app/[locale]/interfaces';
import { POST } from '@/app/[locale]/constants';
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
  try {
    const { url } = createRequestUrl('/permissions');
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

    revalidateTag('/admin/permissions');
    revalidateTag('/permissions/select-all');

    return createSuccessResponse(null);
  } catch (e) {
    return createErrorResponse(e);
  }
}
