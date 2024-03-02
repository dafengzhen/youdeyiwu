'use server';

import { type IError } from '@/app/interfaces';
import { PUT } from '@/app/constants';
import {
  createErrorResponse,
  createRequest,
  createRequestUrl,
  createSuccessResponse,
} from '@/app/common/response';

export interface IUpdatePointActionVariables {
  enable?: boolean;
  initPoints?: number;
}

export default async function UpdatePointConfigAction(
  variables: IUpdatePointActionVariables,
) {
  try {
    const { url } = createRequestUrl('/configs/point');
    const response = await createRequest({
      url,
      options: {
        method: PUT,
        body: variables,
        cache: 'no-store',
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
