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

export interface IUpdateSubmenuActionVariables {
  name?: string;
  link?: string;
  sort?: number;
  menu?: number;
  actions?: number[];
}

export default async function UpdateSubmenuAction({
  id,
  variables,
}: {
  id: number;
  variables: IUpdateSubmenuActionVariables;
}) {
  try {
    const { url } = createRequestUrl(`/submenus/${id}`);
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

    revalidateTag('/admin/submenus');
    revalidateTag(`/admin/submenus/${id}`);

    return createSuccessResponse(null);
  } catch (e) {
    return createErrorResponse(e);
  }
}
