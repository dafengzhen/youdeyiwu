'use server';

import { type IError } from '@/app/interfaces';
import { POST } from '@/app/constants';
import { getXRealIp } from '@/app/common/server';
import {
  createErrorResponse,
  createRequest,
  createRequestUrl,
  createSuccessResponse,
} from '@/app/common/response';

export default async function ViewPagePostAction(variables: {
  id: number | string;
}) {
  try {
    const id = getXRealIp();
    if (!id) {
      return createErrorResponse(
        'The IP address is empty, skip recording post page views',
      );
    }

    const { url } = createRequestUrl(
      `/posts/${variables.id}/view-page?ip=${id}`,
    );
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

    return createSuccessResponse(null);
  } catch (e) {
    return createErrorResponse(e);
  }
}
