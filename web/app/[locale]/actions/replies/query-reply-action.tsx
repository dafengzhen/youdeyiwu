'use server';

import type { IError } from '@/app/[locale]/interfaces';
import type { IReply } from '@/app/[locale]/interfaces/replies';
import {
  createErrorResponse,
  createRequest,
  createRequestUrl,
  createSuccessResponse,
} from '@/app/[locale]/common/response';

export default async function QueryReplyAction(variables: {
  id: number | string;
}) {
  try {
    const { url, str } = createRequestUrl(`/replies/${variables.id}`);
    const response = await createRequest({
      url,
      options: {
        next: {
          tags: [`/admin/replies/${variables.id}`],
        },
      },
    });

    const data = (await response.json()) as IReply | IError;
    if (!response.ok) {
      return createErrorResponse(data);
    }

    return createSuccessResponse(data as IReply);
  } catch (e) {
    return createErrorResponse(e);
  }
}
