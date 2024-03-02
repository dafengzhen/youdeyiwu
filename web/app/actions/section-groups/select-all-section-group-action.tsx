'use server';

import { type IError } from '@/app/interfaces';
import type { ISectionGroup } from '@/app/interfaces/section-groups';
import {
  createErrorResponse,
  createRequest,
  createRequestUrl,
  createSuccessResponse,
} from '@/app/common/response';

export default async function SelectAllSectionGroupAction() {
  try {
    const { url } = createRequestUrl('/section-groups/select-all');
    const response = await createRequest({
      url,
      options: {
        next: {
          tags: ['/section-groups/select-all'],
        },
      },
    });

    const data = (await response.json()) as ISectionGroup[] | IError;
    if (!response.ok) {
      return createErrorResponse(data);
    }

    return createSuccessResponse(data as ISectionGroup[]);
  } catch (e) {
    return createErrorResponse(e);
  }
}
