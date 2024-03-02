'use server';

import { type IError } from '@/app/interfaces';
import { PUT } from '@/app/constants';
import { revalidateTag } from 'next/cache';
import {
  createErrorResponse,
  createRequest,
  createRequestUrl,
  createSuccessResponse,
} from '@/app/common/response';

export interface IUpdateTagGroupActionVariables {
  name?: string;
  sort?: number;
}

export default async function UpdateTagGroupAction({
  id,
  variables,
}: {
  id: number;
  variables: IUpdateTagGroupActionVariables;
}) {
  try {
    const { url, str } = createRequestUrl(`/tag-groups/${id}`);
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
