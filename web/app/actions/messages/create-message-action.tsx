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

export interface ICreateMessageActionVariables {
  name: string;
  overview: string;
  link?: string;
  content?: Record<string, any>;
  receiver: number;
}

export default async function CreateMessageAction(
  variables: ICreateMessageActionVariables,
) {
  try {
    const { url } = createRequestUrl('/messages');
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

    revalidateTag('/admin/messages');

    return createSuccessResponse(null);
  } catch (e) {
    return createErrorResponse(e);
  }
}
