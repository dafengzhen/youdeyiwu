'use server';

import type { IError } from '@/app/[locale]/interfaces';
import {
  createErrorResponse,
  createRequest,
  createRequestUrl,
  createSuccessResponse,
} from '@/app/[locale]/common/response';
import { IFile } from '@/app/[locale]/interfaces/file';

export default async function QueryImagesFileAction(variables: {
  userId: number | string;
}) {
  try {
    const { url } = createRequestUrl(`/files/images/users/${variables.userId}`);
    const response = await createRequest({
      url,
      options: {
        next: {
          tags: ['/admin/files/images', variables.userId + ''],
        },
      },
    });

    const data = (await response.json()) as IFile[] | IError;
    if (!response.ok) {
      return createErrorResponse(data);
    }

    return createSuccessResponse(data as IFile[]);
  } catch (e) {
    return createErrorResponse(e);
  }
}
