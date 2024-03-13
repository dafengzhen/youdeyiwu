'use server';

import type { IError } from '@/app/[locale]/interfaces';
import type { ITag } from '@/app/[locale]/interfaces/tags';
import {
  createErrorResponse,
  createRequest,
  createRequestUrl,
  createSuccessResponse,
} from '@/app/[locale]/common/response';

export default async function QueryTagAction(variables: {
  id: number | string;
}) {
  try {
    const { url, str } = createRequestUrl(`/tags/${variables.id}`);
    const response = await createRequest({
      url,
      options: {
        next: {
          tags: [`/admin/tags/${variables.id}`],
        },
      },
    });

    const data = (await response.json()) as ITag | IError;
    if (!response.ok) {
      return createErrorResponse(data);
    }

    return createSuccessResponse(data as ITag);
  } catch (e) {
    return createErrorResponse(e);
  }
}
