'use server';

import { type IError } from '@/app/interfaces';
import { POST } from '@/app/constants';
import { revalidateTag } from 'next/cache';
import {
  createErrorResponse,
  createRequest,
  createRequestUrl,
  createSuccessResponse,
} from '@/app/common/response';

export interface ICreateSubmenuActionVariables {
  name?: string;
  link?: string;
  sort?: number;
}

export default async function CreateSubmenuAction(
  variables: ICreateSubmenuActionVariables,
) {
  try {
    const { url } = createRequestUrl('/submenus');
    const response = await createRequest({
      url,
      options: {
        method: POST,
        body: variables,
      },
    });

    if (!response.ok) {
      const data = (await response.json()) as IError;
      return createErrorResponse(data);
    }

    revalidateTag('/admin/submenus');

    return createSuccessResponse(null);
  } catch (e) {
    return createErrorResponse(e);
  }
}
