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

export interface IDisableCommentReplyUserActionVariables {
  noPostingAllowed?: boolean;
  disableComments?: boolean;
  disableReplies?: boolean;
  noPostingReason?: string;
  commentDisableReason?: string;
  replyDisableReason?: string;
}

export default async function DisableCommentReplyUserAction({
  id,
  variables,
}: {
  id: number;
  variables: IDisableCommentReplyUserActionVariables;
}) {
  try {
    const { url, str } = createRequestUrl(`/users/${id}/disable-comment-reply`);
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

    revalidateTag('/admin/users');
    revalidateTag(`/admin/users/${id}`);

    return createSuccessResponse(null);
  } catch (e) {
    return createErrorResponse(e);
  }
}
