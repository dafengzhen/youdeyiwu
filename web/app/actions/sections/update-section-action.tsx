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

export interface IUpdateSectionActionVariables {
  name?: string;
  cover?: string;
  overview?: string;
  content?: string;
  sort?: number;
}

export default async function UpdateSectionAction({
  id,
  variables,
}: {
  id: number;
  variables: IUpdateSectionActionVariables;
}) {
  try {
    const { url } = createRequestUrl(`/sections/${id}`);
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

    revalidateTag('/admin/sections');
    revalidateTag(`/admin/sections/${id}`);

    return createSuccessResponse(null);
  } catch (e) {
    return createErrorResponse(e);
  }
}
