'use server';

import type { IError } from '@/app/[locale]/interfaces';
import type { ISectionDetails } from '@/app/[locale]/interfaces/sections';
import {
  createErrorResponse,
  createRequest,
  createRequestUrl,
  createSuccessResponse,
} from '@/app/[locale]/common/response';

export default async function QueryDetailsSectionAction(variables: {
  id: number | string;
}) {
  try {
    const { url } = createRequestUrl(`/sections/${variables.id}/details`);
    const response = await createRequest({
      url,
      options: {
        next: {
          tags: [`/admin/sections/${variables.id}/details`],
        },
      },
    });

    const data = (await response.json()) as ISectionDetails | IError;
    if (!response.ok) {
      return createErrorResponse(data);
    }

    return createSuccessResponse(data as ISectionDetails);
  } catch (e) {
    return createErrorResponse(e);
  }
}
