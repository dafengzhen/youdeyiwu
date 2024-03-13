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

export default async function CreateSectionGroupAction(variables: {
  name: string;
}) {
  try {
    const { url, str } = createRequestUrl('/section-groups');
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

    revalidateTag('/admin/section-groups');

    return createSuccessResponse(null);
  } catch (e) {
    return createErrorResponse(e);
  }
}
