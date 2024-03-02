'use server';

import type { IError } from '@/app/interfaces';
import type { IPointPermissionRule } from '@/app/interfaces/points';
import {
  createErrorResponse,
  createRequest,
  createRequestUrl,
  createSuccessResponse,
} from '@/app/common/response';

export default async function QueryPermissionRulesPointsAction() {
  try {
    const { url, str } = createRequestUrl('/points/permission-rules');
    const response = await createRequest({
      url,
      options: {
        next: {
          tags: ['/admin/points/permission-rules'],
        },
      },
    });

    const data = (await response.json()) as IPointPermissionRule[] | IError;
    if (!response.ok) {
      return createErrorResponse(data);
    }

    return createSuccessResponse(data as IPointPermissionRule[]);
  } catch (e) {
    return createErrorResponse(e);
  }
}
