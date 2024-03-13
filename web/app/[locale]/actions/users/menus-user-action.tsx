'use server';

import type { IError } from '@/app/[locale]/interfaces';
import type { IMenu } from '@/app/[locale]/interfaces/menus';
import {
  createErrorResponse,
  createRequest,
  createRequestUrl,
  createSuccessResponse,
} from '@/app/[locale]/common/response';

export default async function MenusUserAction() {
  try {
    const { url } = createRequestUrl('/users/menus');
    const response = await createRequest({
      url,
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
