'use server';

import type { IError } from '@/app/interfaces';
import type { IJwtConfig } from '@/app/interfaces/configs';
import {
  createErrorResponse,
  createRequest,
  createRequestUrl,
  createSuccessResponse,
} from '@/app/common/response';

export default async function QueryJwtConfigAction() {
  try {
    const { url } = createRequestUrl('/configs/jwt');
    const response = await createRequest({
      url,
    });

    const data = (await response.json()) as IJwtConfig | IError;
    if (!response.ok) {
      return createErrorResponse(data);
    }

    return createSuccessResponse(data as IJwtConfig);
  } catch (e) {
    return createErrorResponse(e);
  }
}
