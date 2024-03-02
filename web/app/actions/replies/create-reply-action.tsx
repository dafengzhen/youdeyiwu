'use server';

import { type IError } from '@/app/interfaces';
import { POST } from '@/app/constants';
import { revalidateTag } from 'next/cache';
import {
  createErrorResponse,
  createRequest,
  createRequestUrl,
  createSuccessResponse,
} from '@/app/common/response';

export interface ICreateReplyActionVariables {
  content: string;
  commentId?: number;
  replyId?: number;
  postId: number;
}

export default async function CreateReplyAction(
  variables: ICreateReplyActionVariables,
) {
  try {
    const { url, str } = createRequestUrl('/replies');
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
