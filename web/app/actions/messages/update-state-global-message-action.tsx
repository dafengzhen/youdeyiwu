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

export default async function UpdateStateGlobalMessageAction({
  id,
}: {
  id: number;
}) {
  try {
    const { url } = createRequestUrl(`/messages/${id}/global-messages/state`);
    const response = await createRequest({
      url,
      options: {
        method: PUT,
        cache: 'no-store',
      },
    });

    if (!response.ok) {
      const data = (await response.json()) as IError;
      return createErrorResponse(data);
    }

    revalidateTag('/admin/messages/global-messages');
    revalidateTag(`/admin/messages/global-messages/${id}`);

    return createSuccessResponse(null);
  } catch (e) {
    return createErrorResponse(e);
  }
}
