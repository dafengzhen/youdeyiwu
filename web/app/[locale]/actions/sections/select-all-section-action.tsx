'use server';

import type { IError } from '@/app/[locale]/interfaces';
import type { ISection } from '@/app/[locale]/interfaces/sections';
import {
  createErrorResponse,
  createRequest,
  createRequestUrl,
  createSuccessResponse,
} from '@/app/[locale]/common/response';

export default async function SelectAllSectionAction(variables?: {
  sectionKey?: string;
}) {
  try {
    const queryParams = variables ?? {};
    const { url, str } = createRequestUrl('/sections/select-all', queryParams);
    const response = await createRequest({
      url,
      options: {
        next: {
          tags: ['/sections/select-all', str],
        },
      },
    });

    const data = (await response.json()) as ISection[] | IError;
    if (!response.ok) {
      return createErrorResponse(data);
    }

    return createSuccessResponse(data as ISection[]);
  } catch (e) {
    return createErrorResponse(e);
  }
}
