'use server';

import { type IError } from '@/app/interfaces';
import FetchDataException from '@/app/exception/fetch-data-exception';
import { AUTHENTICATION_HEADER, JSON_HEADER, POST } from '@/app/constants';
import { revalidateTag } from 'next/cache';
import { checkResponseStatus } from '@/app/common/server';
import { PermissionRuleNameEnum } from '@/app/interfaces/points';

export interface ISavePermissionRulesPointsActionVariables {
  permissionRuleName: PermissionRuleNameEnum;
  requiredPoints?: number;
  operationCost?: number;
}

export default async function SavePermissionRulesPointsAction(
  variables: ISavePermissionRulesPointsActionVariables,
) {
  const response = await fetch(
    process.env.API_SERVER + '/points/permission-rules',
    {
      method: POST,
      headers: {
        ...AUTHENTICATION_HEADER(),
        ...JSON_HEADER,
      },
      body: JSON.stringify(variables),
    },
  );

  if (!response.ok) {
    const data = (await response.json()) as IError;
    checkResponseStatus(response.status);
    throw FetchDataException(data.message);
  }

  revalidateTag('/admin/points/permission-rules');
}
