'use server';

import type { IError } from '@/app/[locale]/interfaces';
import type { IMenu } from '@/app/[locale]/interfaces/menus';
import {
  createErrorResponse,
  createRequest,
  createRequestUrl,
  createSuccessResponse,
} from '@/app/[locale]/common/response';

export default async function QueryMenuAction(variables: {
  id: number | string;
}) {
  try {
    const { url } = createRequestUrl(`/menus/${variables.id}`);
    const response = await createRequest({
      url,
      options: {
        next: {
          tags: [`/admin/menus/${variables.id}`],
        },
      },
    });

    const data = (await response.json()) as IMenu | IError;
    if (!response.ok) {
      return createErrorResponse(data);
    }

    return createSuccessResponse(data as IMenu);
  } catch (e) {
    return createErrorResponse(e);
  }
}
