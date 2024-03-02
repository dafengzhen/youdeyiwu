'use server';

import type { IError } from '@/app/interfaces';
import type { IPointRule } from '@/app/interfaces/points';
import {
  createErrorResponse,
  createRequest,
  createRequestUrl,
  createSuccessResponse,
} from '@/app/common/response';

export default async function QueryRulesPointsAction() {
  try {
    const { url, str } = createRequestUrl('/points/rules');
    const response = await createRequest({
      url,
      options: {
        next: {
          tags: ['/admin/points/rules'],
        },
      },
    });

    const data = (await response.json()) as IPointRule[] | IError;
    if (!response.ok) {
      return createErrorResponse(data);
    }

    return createSuccessResponse(data as IPointRule[]);
  } catch (e) {
    return createErrorResponse(e);
  }
}
