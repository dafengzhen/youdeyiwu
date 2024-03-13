'use server';

import type { IError } from '@/app/[locale]/interfaces';
import type { IPostDetails } from '@/app/[locale]/interfaces/posts';
import {
  createErrorResponse,
  createRequest,
  createRequestUrl,
  createSuccessResponse,
} from '@/app/[locale]/common/response';

export default async function QueryDetailsPostAction(variables: {
  id: number | string;
}) {
  try {
    const { url } = createRequestUrl(`/posts/${variables.id}/details`);
    const response = await createRequest({
      url,
      options: {
        next: {
          tags: [`/admin/posts/${variables.id}/details`],
        },
      },
    });

    const data = (await response.json()) as IPostDetails | IError;
    if (!response.ok) {
      return createErrorResponse(data);
    }

    return createSuccessResponse(data as IPostDetails);
  } catch (e) {
    return createErrorResponse(e);
  }
}
