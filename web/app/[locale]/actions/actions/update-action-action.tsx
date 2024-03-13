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

export interface IUpdateActionActionVariables {
  name?: string;
  alias?: string;
  sort?: number;
  menu?: number;
  submenu?: number;
}

export default async function UpdateActionAction({
  id,
  variables,
}: {
  id: number;
  variables: IUpdateActionActionVariables;
}) {
  try {
    const { url, str } = createRequestUrl(`/actions/${id}`);
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

    revalidateTag('/admin/actions');
    revalidateTag(`/admin/actions/${id}`);

    return createSuccessResponse(null);
  } catch (e) {
    return createErrorResponse(e);
  }
}
