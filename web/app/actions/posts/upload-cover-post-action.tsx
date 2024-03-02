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

export interface IUploadCoverPostActionVariables {
  formData: FormData;
}

export default async function UploadCoverPostAction({
  id,
  variables,
}: {
  id: number;
  variables: IUploadCoverPostActionVariables;
}) {
  try {
    const { url } = createRequestUrl(`/posts/${id}/upload-cover`);
    const response = await createRequest({
      url,
      options: {
        method: POST,
        body: variables.formData,
        skipBody: true,
      },
    });

    if (!response.ok) {
      const data = (await response.json()) as IError;
      return createErrorResponse(data);
    }

    revalidateTag(`/admin/posts/${id}`);

    return createSuccessResponse(null);
  } catch (e) {
    return createErrorResponse(e);
  }
}
