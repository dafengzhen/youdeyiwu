'use server';

import type { IError, IPage, TQueryParams } from '@/app/interfaces';
import type { ICommentReply } from '@/app/interfaces/posts';
import {
  createErrorResponse,
  createRequest,
  createRequestUrl,
  createSuccessResponse,
} from '@/app/common/response';

export default async function CommentReplyPostAction({
  id,
  queryParams,
}: {
  id: string | number;
  queryParams?: TQueryParams;
}) {
  try {
    const { url, str } = createRequestUrl(
      `/posts/${id}/comment-reply`,
      queryParams,
    );
    const response = await createRequest({
      url,
      options: {
        next: {
          tags: [`/posts/${id}/comment-reply`, str],
        },
      },
    });

    const data = (await response.json()) as IPage<ICommentReply[]> | IError;
    if (!response.ok) {
      return createErrorResponse(data);
    }

    return createSuccessResponse(data as IPage<ICommentReply[]>);
  } catch (e) {
    return createErrorResponse(e);
  }
}
