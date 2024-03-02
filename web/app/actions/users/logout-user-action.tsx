'use server';

import { type IError } from '@/app/interfaces';
import { POST } from '@/app/constants';
import { revalidateTag } from 'next/cache';
import { deleteTicket } from '@/app/common/server';
import {
  createErrorResponse,
  createRequest,
  createRequestUrl,
  createSuccessResponse,
} from '@/app/common/response';

export default async function LogoutUserAction(variables: {
  id: number | string;
}) {
  try {
    const { url } = createRequestUrl('/users/logout');
    const response = await createRequest({
      url,
      options: {
        method: POST,
        cache: 'no-store',
      },
    });

    if (!response.ok) {
      const data = (await response.json()) as IError;
      return createErrorResponse(data);
    }

    revalidateTag('/admin/users');
    revalidateTag(`/admin/users/${variables.id}`);
    deleteTicket();

    return createSuccessResponse(null);
  } catch (e) {
    return createErrorResponse(e);
  }
}
