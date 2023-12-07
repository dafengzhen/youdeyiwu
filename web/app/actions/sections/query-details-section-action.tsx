'use server';

import { type IError } from '@/app/interfaces';
import FetchDataException from '@/app/exception/fetch-data-exception';
import { AUTHENTICATION_HEADER } from '@/app/constants';
import { checkResponseStatus } from '@/app/common/server';
import { ISectionDetails } from '@/app/interfaces/sections';

export default async function QueryDetailsSectionAction(variables: {
  id: number | string;
}) {
  const response = await fetch(
    process.env.API_SERVER + `/sections/${variables.id}/details`,
    {
      headers: AUTHENTICATION_HEADER(),
      next: {
        tags: [`/admin/sections/${variables.id}/details`],
      },
    },
  );

  const data = (await response.json()) as ISectionDetails | IError;
  if (!response.ok) {
    checkResponseStatus(response.status);
    throw FetchDataException((data as IError).message);
  }

  return data as ISectionDetails;
}
