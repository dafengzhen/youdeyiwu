'use server';

import type { IError } from '@/app/[locale]/interfaces';
import { POST } from '@/app/[locale]/constants';
import { revalidateTag } from 'next/cache';
import type { RuleNameEnum } from '@/app/[locale]/interfaces/points';
import {
  createErrorResponse,
  createRequest,
  createRequestUrl,
  createSuccessResponse,
} from '@/app/[locale]/common/response';

export interface ISaveRulesPointsActionVariables {
  ruleName: RuleNameEnum;
  initiatorRewardPoints?: number;
  receiverRewardPoints?: number;
  enable?: boolean;
}

export default async function SaveRulesPointsAction(
  variables: ISaveRulesPointsActionVariables,
) {
  try {
    const { url, str } = createRequestUrl('/points/rules');
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

    revalidateTag('/admin/points/rules');

    return createSuccessResponse(null);
  } catch (e) {
    return createErrorResponse(e);
  }
}
