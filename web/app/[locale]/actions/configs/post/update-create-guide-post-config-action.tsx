'use server';

import { type IError } from '@/app/[locale]/interfaces';
import { PUT } from '@/app/[locale]/constants';
import {
  createErrorResponse,
  createRequest,
  createRequestUrl,
  createSuccessResponse,
} from '@/app/[locale]/common/response';

export interface IUpdateCreateGuidePostConfigActionVariables {
  createGuide?: string;
}

export default async function UpdateCreateGuidePostConfigAction(
  variables: IUpdateCreateGuidePostConfigActionVariables,
) {
  try {
    const { url } = createRequestUrl('/configs/post/create-guide');
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
