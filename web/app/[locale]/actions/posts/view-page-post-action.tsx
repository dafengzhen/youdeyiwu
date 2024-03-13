'use server';

import { type IError } from '@/app/[locale]/interfaces';
import { POST } from '@/app/[locale]/constants';
import {
  createErrorResponse,
  createRequest,
  createRequestUrl,
  createSuccessResponse,
} from '@/app/[locale]/common/response';
import { headers } from 'next/headers';

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

const getXRealIp = () => {
  let xRealIp = headers().get('x-real-ip');
  const xForwardedFor = headers().get('x-forwarded-for');

  if (!xRealIp && xForwardedFor) {
    xRealIp = xForwardedFor?.split(',').at(0) ?? 'Unknown';
  }

  return xRealIp;
};
