'use server';

import type { IError } from '@/app/interfaces';
import type { IMenu } from '@/app/interfaces/menus';
import {
  createErrorResponse,
  createRequest,
  createRequestUrl,
  createSuccessResponse,
} from '@/app/common/response';

export default async function QueryAllMenuAction() {
  try {
    const { url } = createRequestUrl('/menus');
    const response = await createRequest({
      url,
      options: {
        next: {
          tags: ['/admin/menus'],
        },
      },
    });

    const data = (await response.json()) as IMenu[] | IError;
    if (!response.ok) {
      return createErrorResponse(data);
    }

    return createSuccessResponse(data as IMenu[]);
  } catch (e) {
    return createErrorResponse(e);
  }
}
