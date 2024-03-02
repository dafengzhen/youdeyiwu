'use server';

import { type IError } from '@/app/interfaces';
import {
  createErrorResponse,
  createRequest,
  createRequestUrl,
  createSuccessResponse,
} from '@/app/common/response';

export default async function GenerateRandomSecretJwtConfigAction() {
  try {
    const { url } = createRequestUrl('/configs/jwt/generate-random-secret');
    const response = await createRequest({
      url,
    });

    if (!response.ok) {
      const data = (await response.json()) as IError;
      return createErrorResponse(data);
    }

    return createSuccessResponse(await response.text());
  } catch (e) {
    return createErrorResponse(e);
  }
}
