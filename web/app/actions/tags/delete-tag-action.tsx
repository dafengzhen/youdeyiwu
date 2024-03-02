'use server';

import { type IError } from '@/app/interfaces';
import { DELETE } from '@/app/constants';
import {
  createErrorResponse,
  createRequest,
  createRequestUrl,
  createSuccessResponse,
} from '@/app/common/response';

export default async function DeleteTagAction(variables: { id: number }) {
  try {
    const { url } = createRequestUrl(`/tags/${variables.id}`);
    const response = await createRequest({
      url,
      options: {
        method: DELETE,
      },
    });

    if (!response.ok) {
      const data = (await response.json()) as IError;
      return createErrorResponse(data);
    }

    return createSuccessResponse(null);
  } catch (e) {
    return createErrorResponse(e);
  }
}
