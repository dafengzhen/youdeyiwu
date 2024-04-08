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

export interface IDisableCommentReplyUserPostActionVariables {
  disableComments?: boolean;
  disableReplies?: boolean;
  commentDisableReason?: string;
  replyDisableReason?: string;
}

export default async function DisableCommentReplyUserPostAction({
  id,
  userId,
  variables,
}: {
  id: number;
  userId: number;
  variables: IDisableCommentReplyUserPostActionVariables;
}) {
  try {
    const { url, str } = createRequestUrl(
      `/posts/${id}/users/${userId}/disable-comment-reply`,
    );
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
