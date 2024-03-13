'use server';

import { type IError } from '@/app/[locale]/interfaces';
import { POST } from '@/app/[locale]/constants';
import { revalidateTag } from 'next/cache';
import {
  createErrorResponse,
  createRequest,
  createRequestUrl,
  createSuccessResponse,
} from '@/app/[locale]/common/response';

export interface ICreateCommentActionVariables {
  content: string;
  postId: number;
}

export default async function CreateCommentAction(
  variables: ICreateCommentActionVariables,
) {
  try {
    const { url } = createRequestUrl('/comments');
    const response = await createRequest({
      url,
      options: {
        method: POST,
        body: variables,
        cache: 'no-store',
      },
    });

    if (!response.ok) {
      const data = (await response.json()) as IError;
      return createErrorResponse(data);
    }

    revalidateTag(`/admin/posts/${variables.postId}/details`);

    return createSuccessResponse(null);
  } catch (e) {
    return createErrorResponse(e);
  }
}
