'use server';

import type { IError } from '@/app/interfaces';
import type { IPointConfig } from '@/app/interfaces/configs';
import {
  createErrorResponse,
  createRequest,
  createRequestUrl,
  createSuccessResponse,
} from '@/app/common/response';

export default async function QueryPointConfigAction() {
  try {
    const { url } = createRequestUrl('/configs/point');
    const response = await createRequest({
      url,
    });

    const data = (await response.json()) as IPointConfig | IError;
    if (!response.ok) {
      return createErrorResponse(data);
    }

    return createSuccessResponse(data as IPointConfig);
  } catch (e) {
    return createErrorResponse(e);
  }
}
