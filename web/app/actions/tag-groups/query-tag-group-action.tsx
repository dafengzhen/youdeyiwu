'use server';

import { type IError } from '@/app/interfaces';
import FetchDataException from '@/app/exception/fetch-data-exception';
import { ITagGroup } from '@/app/interfaces/tag-groups';
import { AUTHENTICATION_HEADER } from '@/app/constants';
import { checkResponseStatus } from '@/app/common/server';

export default async function QueryTagGroupAction(variables: {
  id: number | string;
}) {
  const response = await fetch(
    process.env.API_SERVER + `/tag-groups/${variables.id}`,
    {
      headers: AUTHENTICATION_HEADER(),
      next: {
        tags: [`/admin/tag-groups/${variables.id}`],
      },
    },
  );

  const data = (await response.json()) as ITagGroup | IError;
  if (!response.ok) {
    checkResponseStatus(response.status);
    throw FetchDataException((data as IError).message);
  }

  return data as ITagGroup;
}
