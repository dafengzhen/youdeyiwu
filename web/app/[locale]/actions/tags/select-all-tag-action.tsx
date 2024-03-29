'use server';

import { type IError } from '@/app/[locale]/interfaces';
import type { ITag } from '@/app/[locale]/interfaces/tags';
import {
  createErrorResponse,
  createRequest,
  createRequestUrl,
  createSuccessResponse,
} from '@/app/[locale]/common/response';

export default async function SelectAllTagAction() {
  try {
    const { url } = createRequestUrl('/tags/select-all');
    const response = await createRequest({
      url,
      options: {
        next: {
          tags: ['/tags/select-all'],
        },
      },
    });

    const data = (await response.json()) as ITag[] | IError;
    if (!response.ok) {
      return createErrorResponse(data);
    }

    return createSuccessResponse(data as ITag[]);
  } catch (e) {
    return createErrorResponse(e);
  }
}
