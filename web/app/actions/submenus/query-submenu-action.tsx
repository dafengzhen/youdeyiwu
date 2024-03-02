'use server';

import type { IError } from '@/app/interfaces';
import type { ISubmenu } from '@/app/interfaces/menus';
import {
  createErrorResponse,
  createRequest,
  createRequestUrl,
  createSuccessResponse,
} from '@/app/common/response';

export default async function QuerySubmenuAction(variables: {
  id: number | string;
}) {
  try {
    const { url } = createRequestUrl(`/submenus/${variables.id}`);
    const response = await createRequest({
      url,
      options: {
        next: {
          tags: [`/admin/submenus/${variables.id}`],
        },
      },
    });

    const data = (await response.json()) as ISubmenu | IError;
    if (!response.ok) {
      return createErrorResponse(data);
    }

    return createSuccessResponse(data as ISubmenu);
  } catch (e) {
    return createErrorResponse(e);
  }
}
