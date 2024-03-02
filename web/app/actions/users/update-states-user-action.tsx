'use server';

import { type IError } from '@/app/interfaces';
import { PUT } from '@/app/constants';
import { revalidateTag } from 'next/cache';
import {
  createErrorResponse,
  createRequest,
  createRequestUrl,
  createSuccessResponse,
} from '@/app/common/response';

export interface IUpdateStatesUserActionVariables {
  accountNonExpired?: boolean;
  credentialsNonExpired?: boolean;
  accountNonLocked?: boolean;
  enabled?: boolean;
}

export default async function UpdateStatesUserAction({
  id,
  variables,
}: {
  id: number;
  variables: IUpdateStatesUserActionVariables;
}) {
  try {
    const { url } = createRequestUrl(`/users/${id}/states`);
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
