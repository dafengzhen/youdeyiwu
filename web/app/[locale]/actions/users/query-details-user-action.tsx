'use server';

import type { IError } from '@/app/[locale]/interfaces';
import type { IUserDetails } from '@/app/[locale]/interfaces/users';
import {
  createErrorResponse,
  createRequest,
  createRequestUrl,
  createSuccessResponse,
} from '@/app/[locale]/common/response';

export default async function QueryDetailsUserAction(variables: {
  id: number | string;
}) {
  try {
    if (typeof variables.id === 'string' && variables.id.includes('.')) {
      return createErrorResponse('Invalid Id', 'Bad Request', null, 400);
    }

    const { url } = createRequestUrl(`/users/${variables.id}/details`);
    const response = await createRequest({
      url,
      options: {
        next: {
          tags: [`/admin/users/${variables.id}/details`],
        },
      },
    });

    const data = (await response.json()) as IUserDetails | IError;
    if (!response.ok) {
      return createErrorResponse(data);
    }

    return createSuccessResponse(data as IUserDetails);
  } catch (e) {
    return createErrorResponse(e);
  }
}
