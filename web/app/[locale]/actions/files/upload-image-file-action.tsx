'use server';

import { type IError } from '@/app/[locale]/interfaces';
import { POST } from '@/app/[locale]/constants';
import {
  createErrorResponse,
  createRequest,
  createRequestUrl,
  createSuccessResponse,
} from '@/app/[locale]/common/response';

export default async function UploadImageFileAction(variables: {
  formData: FormData;
}) {
  try {
    const { url } = createRequestUrl('/files/images');
    const response = await createRequest({
      url,
      options: {
        method: POST,
        body: variables.formData,
        skipBody: true,
        skipHeader: true,
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
