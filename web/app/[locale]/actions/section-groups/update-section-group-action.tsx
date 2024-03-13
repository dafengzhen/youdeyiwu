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

export interface IUpdateSectionGroupActionVariables {
  name?: string;
  sort?: number;
}

export default async function UpdateSectionGroupAction({
  id,
  variables,
}: {
  id: number;
  variables: IUpdateSectionGroupActionVariables;
}) {
  try {
    const { url, str } = createRequestUrl(`/section-groups/${id}`);
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

    revalidateTag('/admin/section-groups');
    revalidateTag(`/admin/section-groups/${id}`);

    return createSuccessResponse(null);
  } catch (e) {
    return createErrorResponse(e);
  }
}
