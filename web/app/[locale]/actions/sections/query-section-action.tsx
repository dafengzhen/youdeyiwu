'use server';

import type { IError } from '@/app/[locale]/interfaces';
import type { ISection } from '@/app/[locale]/interfaces/sections';
import {
  createErrorResponse,
  createRequest,
  createRequestUrl,
  createSuccessResponse,
} from '@/app/[locale]/common/response';

export default async function QuerySectionAction(variables: {
  id: number | string;
}) {
  try {
    const { url } = createRequestUrl(`/sections/${variables.id}`);
    const response = await createRequest({
      url,
      options: {
        next: {
          tags: [`/admin/sections/${variables.id}`],
        },
      },
    });

    const data = (await response.json()) as ISection | IError;
    if (!response.ok) {
      return createErrorResponse(data);
    }

    return createSuccessResponse(data as ISection);
  } catch (e) {
    return createErrorResponse(e);
  }
}
