'use server';

import { type IError } from '@/app/[locale]/interfaces';
import { POST } from '@/app/[locale]/constants';
import { revalidateTag } from 'next/cache';
import {
  createErrorResponse,
  createRequest,
  createRequestUrl,
  createSuccessResponse,
} from '@/app/[locale]/common/response';

export interface ICreateMenuActionVariables {
  name?: string;
  link?: string;
  sort?: number;
}

export default async function CreateMenuAction(
  variables: ICreateMenuActionVariables,
) {
  try {
    const { url } = createRequestUrl('/menus');
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

    revalidateTag('/admin/menus');

    return createSuccessResponse(null);
  } catch (e) {
    return createErrorResponse(e);
  }
}
