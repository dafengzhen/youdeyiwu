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

export interface ICreateGlobalMessageActionVariables {
  name: string;
  overview: string;
  link?: string;
  content?: Record<string, any>;
  sort: number;
}

export default async function CreateGlobalMessageAction(
  variables: ICreateGlobalMessageActionVariables,
) {
  try {
    const { url } = createRequestUrl('/messages/global-messages');
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

    revalidateTag('/admin/messages/global-messages');

    return createSuccessResponse(null);
  } catch (e) {
    return createErrorResponse(e);
  }
}
