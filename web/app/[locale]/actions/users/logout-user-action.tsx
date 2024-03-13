'use server';

import { type IError } from '@/app/[locale]/interfaces';
import { POST, SECURE_TK, TK } from '@/app/[locale]/constants';
import { revalidateTag } from 'next/cache';
import {
  createErrorResponse,
  createRequest,
  createRequestUrl,
  createSuccessResponse,
} from '@/app/[locale]/common/response';
import { cookies } from 'next/headers';

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

const deleteTicket = () => {
  const isHttpsSite = process.env.IS_HTTPS_SITE === 'true';
  cookies().delete(isHttpsSite ? SECURE_TK : TK);
};
