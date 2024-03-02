'use server';

import type { IError } from '@/app/interfaces';
import type { TUsersCountByDate } from '@/app/interfaces/users';
import {
  createErrorResponse,
  createRequest,
  createRequestUrl,
  createSuccessResponse,
} from '@/app/common/response';

export default async function CountByDateUserAction(variables?: {
  pastDays?: number;
}) {
  try {
    const { url } = createRequestUrl(
      `/users/count-by-date?pastDays=${variables?.pastDays ?? 15}`,
    );
    const response = await createRequest({
      url,
    });

    const data = (await response.json()) as TUsersCountByDate | IError;
    if (!response.ok) {
      return createErrorResponse(data);
    }

    return createSuccessResponse(data as TUsersCountByDate);
  } catch (e) {
    return createErrorResponse(e);
  }
}
