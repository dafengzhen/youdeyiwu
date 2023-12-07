'use server';

import { type IError } from '@/app/interfaces';
import FetchDataException from '@/app/exception/fetch-data-exception';
import { AUTHENTICATION_HEADER, JSON_HEADER, PUT } from '@/app/constants';
import { revalidateTag } from 'next/cache';
import { checkResponseStatus } from '@/app/common/server';

export interface IUpdateTagsTagGroupActionVariables {
  tags?: number[];
}

export default async function UpdateTagsTagGroupAction({
  id,
  variables,
}: {
  id: number;
  variables: IUpdateTagsTagGroupActionVariables;
}) {
  const response = await fetch(
    process.env.API_SERVER + `/tag-groups/${id}/tags`,
    {
      method: PUT,
      headers: {
        ...AUTHENTICATION_HEADER(),
        ...JSON_HEADER,
      },
      body: JSON.stringify(variables),
      cache: 'no-store',
    },
  );

  if (!response.ok) {
    const data = (await response.json()) as IError;
    checkResponseStatus(response.status);
    throw FetchDataException(data.message);
  }

  revalidateTag('/admin/tag-groups');
  revalidateTag(`/admin/tag-groups/${id}`);
}
