'use server';

import type { IError, IPage, TQueryParams } from '@/app/interfaces';
import type { IGlobalMessage } from '@/app/interfaces/messages';
import {
  createErrorResponse,
  createRequest,
  createRequestUrl,
  createSuccessResponse,
} from '@/app/common/response';

export default async function QueryAllGlobalMessageAction(
  queryParams?: TQueryParams,
) {
  try {
    const { url, str } = createRequestUrl(
      '/messages/global-messages',
      queryParams,
    );
    const response = await createRequest({
      url,
      options: {
        next: {
          tags: ['/admin/messages/global-messages', str],
        },
      },
    });

    const data = (await response.json()) as IPage<IGlobalMessage[]> | IError;
    if (!response.ok) {
      return createErrorResponse(data);
    }

    return createSuccessResponse(data as IPage<IGlobalMessage[]>);
  } catch (e) {
    return createErrorResponse(e);
  }
}
