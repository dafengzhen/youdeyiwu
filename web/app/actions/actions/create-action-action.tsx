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

export interface ICreateActionActionVariables {
  name?: string;
  alias?: string;
  sort?: number;
}

export default async function CreateActionAction(
  variables: ICreateActionActionVariables,
) {
  try {
    const { url, str } = createRequestUrl('/actions');
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

    revalidateTag('/admin/actions');

    return createSuccessResponse(null);
  } catch (e) {
    return createErrorResponse(e);
  }
}
