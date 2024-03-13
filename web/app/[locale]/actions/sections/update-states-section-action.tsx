'use server';

import { type IError } from '@/app/[locale]/interfaces';
import { PUT } from '@/app/[locale]/constants';
import { ISectionState } from '@/app/[locale]/interfaces/sections';
import { revalidateTag } from 'next/cache';
import {
  createErrorResponse,
  createRequest,
  createRequestUrl,
  createSuccessResponse,
} from '@/app/[locale]/common/response';

export interface IUpdateStatesSectionActionVariables {
  states?: ISectionState[];
  allows?: number[];
  blocks?: number[];
  accessKey?: string;
}

export default async function UpdateStatesSectionAction({
  id,
  variables,
}: {
  id: number;
  variables: IUpdateStatesSectionActionVariables;
}) {
  try {
    const { url } = createRequestUrl(`/sections/${id}/states`);
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
