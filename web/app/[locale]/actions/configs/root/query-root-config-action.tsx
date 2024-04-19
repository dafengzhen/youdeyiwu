'use server';

import type { IError, TQueryParams } from '@/app/[locale]/interfaces';
import {
  createErrorResponse,
  createRequest,
  createRequestUrl,
  createSuccessResponse,
} from '@/app/[locale]/common/response';
import type { IRootConfig } from '@/app/[locale]/interfaces/configs';

export default async function QueryRootConfigAction(
  queryParams?: TQueryParams,
) {
  try {
    const { url } = createRequestUrl('/configs/root', queryParams);
    const response = await createRequest({
      url,
    });

    const data = (await response.json()) as IRootConfig | IError;
    if (!response.ok) {
      return createErrorResponse(data);
    }

    return createSuccessResponse(data as IRootConfig);
  } catch (e) {
    return createErrorResponse(e);
  }
}
