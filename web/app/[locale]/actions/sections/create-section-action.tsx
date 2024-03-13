'use server';

import { type IError } from '@/app/[locale]/interfaces';
import { POST } from '@/app/[locale]/constants';
import { revalidateTag } from 'next/cache';
import {
  createErrorResponse,
  createRequest,
  createRequestUrl,
  createSuccessResponse,
} from '@/app/[locale]/common/response';

export default async function CreateSectionAction(variables: { name: string }) {
  try {
    const { url } = createRequestUrl('/sections');
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

    revalidateTag('/admin/sections');
    revalidateTag('/sections/select-all');

    return createSuccessResponse(null);
  } catch (e) {
    return createErrorResponse(e);
  }
}
