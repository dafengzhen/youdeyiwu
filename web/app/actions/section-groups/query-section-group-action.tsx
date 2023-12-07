'use server';

import { type IError } from '@/app/interfaces';
import FetchDataException from '@/app/exception/fetch-data-exception';
import { ISectionGroup } from '@/app/interfaces/section-groups';
import { AUTHENTICATION_HEADER } from '@/app/constants';
import { checkResponseStatus } from '@/app/common/server';

export default async function QuerySectionGroupAction(variables: {
  id: number | string;
}) {
  const response = await fetch(
    process.env.API_SERVER + `/section-groups/${variables.id}`,
    {
      headers: AUTHENTICATION_HEADER(),
      next: {
        tags: [`/admin/section-groups/${variables.id}`],
      },
    },
  );

  const data = (await response.json()) as ISectionGroup | IError;
  if (!response.ok) {
    checkResponseStatus(response.status);
    throw FetchDataException((data as IError).message);
  }

  return data as ISectionGroup;
}
