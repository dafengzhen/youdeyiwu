'use server';

import type { IError } from '@/app/interfaces';
import type { IUser } from '@/app/interfaces/users';
import {
  createErrorResponse,
  createRequest,
  createRequestUrl,
  createSuccessResponse,
} from '@/app/common/response';

export default async function QueryUserAction(variables: {
  id: number | string;
}) {
  try {
    const { url } = createRequestUrl(`/users/${variables.id}`);
    const response = await createRequest({
      url,
      options: {
        next: {
          tags: [`/admin/users/${variables.id}`],
        },
      },
    });

    const data = (await response.json()) as IUser | IError;
    if (!response.ok) {
      return createErrorResponse(data);
    }

    return createSuccessResponse(data as IUser);
  } catch (e) {
    return createErrorResponse(e);
  }
}
