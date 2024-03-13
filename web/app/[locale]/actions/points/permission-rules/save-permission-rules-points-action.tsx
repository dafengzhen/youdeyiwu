'use server';

import type { IError } from '@/app/[locale]/interfaces';
import { POST } from '@/app/[locale]/constants';
import { revalidateTag } from 'next/cache';
import type { PermissionRuleNameEnum } from '@/app/[locale]/interfaces/points';
import {
  createErrorResponse,
  createRequest,
  createRequestUrl,
  createSuccessResponse,
} from '@/app/[locale]/common/response';

export interface ISavePermissionRulesPointsActionVariables {
  permissionRuleName: PermissionRuleNameEnum;
  requiredPoints?: number;
  operationCost?: number;
}

export default async function SavePermissionRulesPointsAction(
  variables: ISavePermissionRulesPointsActionVariables,
) {
  try {
    const { url, str } = createRequestUrl('/points/permission-rules');
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

    revalidateTag('/admin/points/permission-rules');

    return createSuccessResponse(null);
  } catch (e) {
    return createErrorResponse(e);
  }
}
