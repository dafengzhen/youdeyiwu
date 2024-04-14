'use server';

import { type IError } from '@/app/[locale]/interfaces';
import { DELETE } from '@/app/[locale]/constants';
import {
  createErrorResponse,
  createRequest,
  createRequestUrl,
  createSuccessResponse,
} from '@/app/[locale]/common/response';

export default async function DeleteImageFileAction(variables: { id: number }) {
  try {
    const { url } = createRequestUrl(`/files/images/${variables.id}`);
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
