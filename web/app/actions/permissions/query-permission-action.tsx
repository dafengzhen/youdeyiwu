'use server';

import type { IError } from '@/app/interfaces';
import type { IPermission } from '@/app/interfaces/permissions';
import {
  createErrorResponse,
  createRequest,
  createRequestUrl,
  createSuccessResponse,
} from '@/app/common/response';

export default async function QueryPermissionAction(variables: {
  id: number | string;
}) {
  try {
    const { url, str } = createRequestUrl(`/permissions/${variables.id}`);
    const response = await createRequest({
      url,
      options: {
        next: {
          tags: [`/admin/permissions/${variables.id}`],
        },
      },
    });

    const data = (await response.json()) as IPermission | IError;
    if (!response.ok) {
      return createErrorResponse(data);
    }

    return createSuccessResponse(data as IPermission);
  } catch (e) {
    return createErrorResponse(e);
  }
}
