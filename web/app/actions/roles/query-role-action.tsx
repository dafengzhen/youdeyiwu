'use server';

import type { IError } from '@/app/interfaces';
import type { IRole } from '@/app/interfaces/roles';
import {
  createErrorResponse,
  createRequest,
  createRequestUrl,
  createSuccessResponse,
} from '@/app/common/response';

export default async function QueryRoleAction(variables: {
  id: number | string;
}) {
  try {
    const { url } = createRequestUrl(`/roles/${variables.id}`);
    const response = await createRequest({
      url,
      options: {
        next: {
          tags: [`/admin/roles/${variables.id}`],
        },
      },
    });

    const data = (await response.json()) as IRole | IError;
    if (!response.ok) {
      return createErrorResponse(data);
    }

    return createSuccessResponse(data as IRole);
  } catch (e) {
    return createErrorResponse(e);
  }
}
