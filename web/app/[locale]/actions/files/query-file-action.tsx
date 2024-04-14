'use server';

import type { IError } from '@/app/[locale]/interfaces';
import {
  createErrorResponse,
  createRequest,
  createRequestUrl,
  createSuccessResponse,
} from '@/app/[locale]/common/response';
import type { IFile } from '@/app/[locale]/interfaces/file';

export default async function QueryFileAction(variables: {
  id: number | string;
}) {
  try {
    const { url } = createRequestUrl(`/files/${variables.id}`);
    const response = await createRequest({
      url,
      options: {
        next: {
          tags: [`/admin/files/${variables.id}`],
        },
      },
    });

    const data = (await response.json()) as IFile | IError;
    if (!response.ok) {
      return createErrorResponse(data);
    }

    return createSuccessResponse(data as IFile);
  } catch (e) {
    return createErrorResponse(e);
  }
}
