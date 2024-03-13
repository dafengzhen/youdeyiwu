'use server';

import type { IError } from '@/app/[locale]/interfaces';
import type { IGlobalMessage } from '@/app/[locale]/interfaces/messages';
import {
  createErrorResponse,
  createRequest,
  createRequestUrl,
  createSuccessResponse,
} from '@/app/[locale]/common/response';

export default async function QueryGlobalMessageAction(variables: {
  id: number | string;
}) {
  try {
    const { url } = createRequestUrl(
      `/messages/global-messages/${variables.id}`,
    );
    const response = await createRequest({
      url,
      options: {
        next: {
          tags: [`/admin/messages/global-messages/${variables.id}`],
        },
      },
    });

    const data = (await response.json()) as IGlobalMessage | IError;
    if (!response.ok) {
      return createErrorResponse(data);
    }

    return createSuccessResponse(data as IGlobalMessage);
  } catch (e) {
    return createErrorResponse(e);
  }
}
