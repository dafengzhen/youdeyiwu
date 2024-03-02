'use server';

import type { IError, IHealth } from '@/app/interfaces';
import {
  createErrorResponse,
  createRequest,
  createRequestUrl,
  createSuccessResponse,
} from '@/app/common/response';

export default async function HealthAction() {
  try {
    const { url, str } = createRequestUrl('/actuator/health');
    const response = await createRequest({
      url,
    });

    const data = (await response.json()) as IHealth | IError;
    if (!response.ok) {
      return createErrorResponse(data);
    }

    return createSuccessResponse(data as IHealth);
  } catch (e) {
    return createErrorResponse(e);
  }
}
