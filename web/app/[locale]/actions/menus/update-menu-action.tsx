'use server';

import { type IError } from '@/app/[locale]/interfaces';
import { PUT } from '@/app/[locale]/constants';
import { revalidateTag } from 'next/cache';
import {
  createErrorResponse,
  createRequest,
  createRequestUrl,
  createSuccessResponse,
} from '@/app/[locale]/common/response';

export interface IUpdateMenuActionVariables {
  name?: string;
  link?: string;
  sort?: number;
  submenus?: number[];
  actions?: number[];
}

export default async function UpdateMenuAction({
  id,
  variables,
}: {
  id: number;
  variables: IUpdateMenuActionVariables;
}) {
  try {
    const { url } = createRequestUrl(`/menus/${id}`);
    const response = await createRequest({
      url,
      options: {
        method: PUT,
        body: variables,
        cache: 'no-store',
      },
    });

    if (!response.ok) {
      const data = (await response.json()) as IError;
      return createErrorResponse(data);
    }

    revalidateTag('/admin/menus');
    revalidateTag(`/admin/menus/${id}`);

    return createSuccessResponse(null);
  } catch (e) {
    return createErrorResponse(e);
  }
}
