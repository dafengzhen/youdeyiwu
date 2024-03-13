'use server';

import type { IError } from '@/app/[locale]/interfaces';
import type { ITagGroup } from '@/app/[locale]/interfaces/tag-groups';
import {
  createErrorResponse,
  createRequest,
  createRequestUrl,
  createSuccessResponse,
} from '@/app/[locale]/common/response';

export default async function QueryTagGroupAction(variables: {
  id: number | string;
}) {
  try {
    const { url, str } = createRequestUrl(`/tag-groups/${variables.id}`);
    const response = await createRequest({
      url,
      options: {
        next: {
          tags: [`/admin/tag-groups/${variables.id}`],
        },
      },
    });

    const data = (await response.json()) as ITagGroup | IError;
    if (!response.ok) {
      return createErrorResponse(data);
    }

    return createSuccessResponse(data as ITagGroup);
  } catch (e) {
    return createErrorResponse(e);
  }
}
