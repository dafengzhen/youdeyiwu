'use server';

import { type IError } from '@/app/interfaces';
import { POST } from '@/app/constants';
import { revalidateTag } from 'next/cache';
import {
  createErrorResponse,
  createRequest,
  createRequestUrl,
  createSuccessResponse,
} from '@/app/common/response';

export default async function CreateTagAction(variables: { name: string }) {
  try {
    const { url } = createRequestUrl('/tags');
    const response = await createRequest({
      url,
      options: {
        method: POST,
        body: variables,
      },
    });

    if (!response.ok) {
      const data = (await response.json()) as IError;
      return createErrorResponse(data);
    }

    revalidateTag('/admin/tags');

    return createSuccessResponse(null);
  } catch (e) {
    return createErrorResponse(e);
  }
}
