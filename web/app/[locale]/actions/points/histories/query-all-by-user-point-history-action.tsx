'use server';

import type { IError, IPage, TQueryParams } from '@/app/[locale]/interfaces';
import {
  createErrorResponse,
  createRequest,
  createRequestUrl,
  createSuccessResponse,
} from '@/app/[locale]/common/response';
import { IPointHistory } from '@/app/[locale]/interfaces/points';

export default async function QueryAllByUserPointHistoryAction(variables: {
  userId: number | string;
  queryParams?: TQueryParams;
}) {
  try {
    const path = `/points/histories/users/${variables.userId}`;
    const { url, str } = createRequestUrl(path, variables.queryParams);
    const response = await createRequest({
      url,
      options: {
        next: {
          tags: [path, str],
        },
      },
    });

    const data = (await response.json()) as IPage<IPointHistory[]> | IError;
    if (!response.ok) {
      return createErrorResponse(data);
    }

    return createSuccessResponse(data as IPage<IPointHistory[]>);
  } catch (e) {
    return createErrorResponse(e);
  }
}
