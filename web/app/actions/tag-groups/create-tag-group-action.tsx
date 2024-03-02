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

export default async function CreateTagGroupAction(variables: {
  name: string;
}) {
  try {
    const { url, str } = createRequestUrl('/tag-groups');
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

    revalidateTag('/admin/tag-groups');

    return createSuccessResponse(null);
  } catch (e) {
    return createErrorResponse(e);
  }
}
