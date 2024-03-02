'use server';

import type { IError } from '@/app/interfaces';
import { PUT } from '@/app/constants';
import type { ISectionState } from '@/app/interfaces/sections';
import { revalidateTag } from 'next/cache';
import type { IPostReviewState, IPostSortState } from '@/app/interfaces/posts';
import {
  createErrorResponse,
  createRequest,
  createRequestUrl,
  createSuccessResponse,
} from '@/app/common/response';

export interface IUpdateStatesPostActionVariables {
  states?: ISectionState[];
  allows?: number[];
  blocks?: number[];
  accessKey?: string;
  reviewState: IPostReviewState;
  sortState: IPostSortState;
  reason?: string;
}

export default async function UpdateStatesPostAction({
  id,
  variables,
}: {
  id: number;
  variables: IUpdateStatesPostActionVariables;
}) {
  try {
    const { url, str } = createRequestUrl(`/posts/${id}/states`);
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

    revalidateTag('/admin/posts');
    revalidateTag(`/admin/posts/${id}`);

    return createSuccessResponse(null);
  } catch (e) {
    return createErrorResponse(e);
  }
}
