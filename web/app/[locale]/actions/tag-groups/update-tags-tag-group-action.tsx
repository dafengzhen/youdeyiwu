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

export interface IUpdateTagsTagGroupActionVariables {
  tags?: number[];
}

export default async function UpdateTagsTagGroupAction({
  id,
  variables,
}: {
  id: number;
  variables: IUpdateTagsTagGroupActionVariables;
}) {
  try {
    const { url, str } = createRequestUrl(`/tag-groups/${id}/tags`);
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

    revalidateTag('/admin/tag-groups');
    revalidateTag(`/admin/tag-groups/${id}`);

    return createSuccessResponse(null);
  } catch (e) {
    return createErrorResponse(e);
  }
}
