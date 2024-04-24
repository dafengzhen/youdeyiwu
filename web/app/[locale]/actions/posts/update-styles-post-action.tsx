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

export interface IUpdateStylesPostActionVariables {
  styles?: string;
  classNames?: string;
}

export default async function UpdateStylesPostAction({
  id,
  variables,
}: {
  id: number;
  variables: IUpdateStylesPostActionVariables;
}) {
  try {
    const { url } = createRequestUrl(`/posts/${id}/styles`);
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

    revalidateTag(`/admin/posts/${id}`);

    return createSuccessResponse(null);
  } catch (e) {
    return createErrorResponse(e);
  }
}
