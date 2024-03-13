'use server';

import type { IError } from '@/app/[locale]/interfaces';
import type { IPointConfig } from '@/app/[locale]/interfaces/configs';
import {
  createErrorResponse,
  createRequest,
  createRequestUrl,
  createSuccessResponse,
} from '@/app/[locale]/common/response';

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
