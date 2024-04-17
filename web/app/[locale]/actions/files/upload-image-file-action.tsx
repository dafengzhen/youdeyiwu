'use server';

import { type IError } from '@/app/[locale]/interfaces';
import { POST } from '@/app/[locale]/constants';
import {
  createErrorResponse,
  createRequest,
  createRequestUrl,
  createSuccessResponse,
} from '@/app/[locale]/common/response';
import type { IFileUrls } from '@/app/[locale]/interfaces/file';

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

    const data = (await response.json()) as IFileUrls | IError;
    if (!response.ok) {
      return createErrorResponse(data);
    }

    return createSuccessResponse(data as IFileUrls);
  } catch (e) {
    return createErrorResponse(e);
  }
}
