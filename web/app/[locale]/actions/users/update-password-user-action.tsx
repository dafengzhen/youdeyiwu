'use server';

import { type IError } from '@/app/[locale]/interfaces';
import { PUT } from '@/app/[locale]/constants';
import { revalidateTag } from 'next/cache';
import {
  createErrorResponse,
  createRequest,
  createRequestUrl,
  createSuccessResponse,
} from '@/app/[locale]/common/response';

export interface IUpdatePasswordUserActionVariables {
  oldPassword: string;
  newPassword: string;
}

export default async function UpdatePasswordUserAction({
  id,
  variables,
}: {
  id: number;
  variables: IUpdatePasswordUserActionVariables;
}) {
  try {
    const { url } = createRequestUrl(`/users/${id}/password`);
    const response = await createRequest({
      url,
      options: {
        method: PUT,
        body: variables,
        cache: 'no-store',
      },
    });

    if (!response.ok) {
      const data = (await response.json()) as IError;
      return createErrorResponse(data);
    }

    revalidateTag('/admin/users');
    revalidateTag(`/admin/users/${id}`);

    return createSuccessResponse(null);
  } catch (e) {
    return createErrorResponse(e);
  }
}
