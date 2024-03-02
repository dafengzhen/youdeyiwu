'use server';

import type { IError } from '@/app/interfaces';
import type { ISubmenu } from '@/app/interfaces/menus';
import {
  createErrorResponse,
  createRequest,
  createRequestUrl,
  createSuccessResponse,
} from '@/app/common/response';

export default async function QueryAllSubmenuAction() {
  try {
    const { url } = createRequestUrl('/submenus');
    const response = await createRequest({
      url,
      options: {
        next: {
          tags: ['/admin/submenus'],
        },
      },
    });

    const data = (await response.json()) as ISubmenu[] | IError;
    if (!response.ok) {
      return createErrorResponse(data);
    }

    return createSuccessResponse(data as ISubmenu[]);
  } catch (e) {
    return createErrorResponse(e);
  }
}
