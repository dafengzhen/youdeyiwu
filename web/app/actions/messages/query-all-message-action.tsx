'use server';

import type { IError, IPage, TQueryParams } from '@/app/interfaces';
import type { IMessage } from '@/app/interfaces/messages';
import {
  createErrorResponse,
  createRequest,
  createRequestUrl,
  createSuccessResponse,
} from '@/app/common/response';

export default async function QueryAllMessageAction(
  queryParams?: TQueryParams,
) {
  try {
    const { url, str } = createRequestUrl('/messages', queryParams);
    const response = await createRequest({
      url,
      options: {
        next: {
          tags: ['/admin/messages', str],
        },
      },
    });

    const data = (await response.json()) as IPage<IMessage[]> | IError;
    if (!response.ok) {
      return createErrorResponse(data);
    }

    return createSuccessResponse(data as IPage<IMessage[]>);
  } catch (e) {
    return createErrorResponse(e);
  }
}
