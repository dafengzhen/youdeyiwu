'use server';

import type { IError } from '@/app/interfaces';
import type { ISectionGroup } from '@/app/interfaces/section-groups';
import {
  createErrorResponse,
  createRequest,
  createRequestUrl,
  createSuccessResponse,
} from '@/app/common/response';

export default async function QuerySectionGroupAction(variables: {
  id: number | string;
}) {
  try {
    const { url, str } = createRequestUrl(`/section-groups/${variables.id}`);
    const response = await createRequest({
      url,
      options: {
        next: {
          tags: [`/admin/section-groups/${variables.id}`],
        },
      },
    });

    const data = (await response.json()) as ISectionGroup | IError;
    if (!response.ok) {
      return createErrorResponse(data);
    }

    return createSuccessResponse(data as ISectionGroup);
  } catch (e) {
    return createErrorResponse(e);
  }
}
