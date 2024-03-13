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

export interface IUpdateSectionsSectionGroupActionVariables {
  sections?: number[];
}

export default async function UpdateSectionsSectionGroupAction({
  id,
  variables,
}: {
  id: number;
  variables: IUpdateSectionsSectionGroupActionVariables;
}) {
  try {
    const { url } = createRequestUrl(`/section-groups/${id}/sections`);
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
