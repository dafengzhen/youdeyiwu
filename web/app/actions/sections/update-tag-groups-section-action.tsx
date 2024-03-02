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

export interface IUpdateTagGroupsSectionActionVariables {
  tagGroups: number[];
}

export default async function UpdateTagGroupsSectionAction({
  id,
  variables,
}: {
  id: number;
  variables: IUpdateTagGroupsSectionActionVariables;
}) {
  try {
    const { url } = createRequestUrl(`/sections/${id}/tag-groups`);
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

    revalidateTag(`/admin/sections/${id}`);

    return createSuccessResponse(null);
  } catch (e) {
    return createErrorResponse(e);
  }
}
