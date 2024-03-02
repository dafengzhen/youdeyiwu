'use server';

import type { IError } from '@/app/interfaces';
import type { IAction } from '@/app/interfaces/menus';
import {
  createErrorResponse,
  createRequest,
  createRequestUrl,
  createSuccessResponse,
} from '@/app/common/response';

export default async function QueryAllActionAction() {
  try {
    const { url, str } = createRequestUrl('/actions');
    const response = await createRequest({
      url,
      options: {
        next: {
          tags: ['/admin/actions'],
        },
      },
    });

    const data = (await response.json()) as IAction[] | IError;
    if (!response.ok) {
      return createErrorResponse(data);
    }

    return createSuccessResponse(data as IAction[]);
  } catch (e) {
    return createErrorResponse(e);
  }
}
