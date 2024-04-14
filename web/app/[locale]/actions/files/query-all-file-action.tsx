'use server';

import type { IError, IPage, TQueryParams } from '@/app/[locale]/interfaces';
import {
  createErrorResponse,
  createRequest,
  createRequestUrl,
  createSuccessResponse,
} from '@/app/[locale]/common/response';
import type { IFile } from '@/app/[locale]/interfaces/file';

export default async function QueryAllFileAction(queryParams?: TQueryParams) {
  try {
    const { url, str } = createRequestUrl('/files', queryParams);
    const response = await createRequest({
      url,
      options: {
        next: {
          tags: ['/admin/files', str],
        },
      },
    });

    const data = (await response.json()) as IPage<IFile[]> | IError;
    if (!response.ok) {
      return createErrorResponse(data);
    }

    return createSuccessResponse(data as IPage<IFile[]>);
  } catch (e) {
    return createErrorResponse(e);
  }
}
