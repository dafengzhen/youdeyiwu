'use server';

import type { IError } from '@/app/interfaces';
import type { IAction } from '@/app/interfaces/menus';
import {
  createErrorResponse,
  createRequest,
  createRequestUrl,
  createSuccessResponse,
} from '@/app/common/response';

export default async function QueryActionAction(variables: {
  id: number | string;
}) {
  try {
    const { url, str } = createRequestUrl(`/actions/${variables.id}`);
    const response = await createRequest({
      url,
      options: {
        next: {
          tags: [`/admin/actions/${variables.id}`],
        },
      },
    });

    const data = (await response.json()) as IAction | IError;
    if (!response.ok) {
      return createErrorResponse(data);
    }

    return createSuccessResponse(data as IAction);
  } catch (e) {
    return createErrorResponse(e);
  }
}
