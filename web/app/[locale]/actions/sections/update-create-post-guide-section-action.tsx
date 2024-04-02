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

export interface IUpdateCreatePostGuideSectionActionVariables {
  createPostGuide?: string;
}

export default async function UpdateCreatePostGuideSectionAction({
  id,
  variables,
}: {
  id: number;
  variables: IUpdateCreatePostGuideSectionActionVariables;
}) {
  try {
    const { url } = createRequestUrl(`/sections/${id}/create-post-guide`);
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
