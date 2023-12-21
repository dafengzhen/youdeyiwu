'use server';

import { type IError } from '@/app/interfaces';
import FetchDataException from '@/app/exception/fetch-data-exception';
import { AUTHENTICATION_HEADER, JSON_HEADER, POST } from '@/app/constants';
import { revalidateTag } from 'next/cache';
import { checkResponseStatus } from '@/app/common/server';

export interface IUploadCoverPostActionVariables {
  file: File;
}

export default async function UploadCoverPostAction({
  id,
  variables,
}: {
  id: number;
  variables: IUploadCoverPostActionVariables;
}) {
  const body = new FormData();
  body.append('file', variables.file);

  const response = await fetch(
    process.env.API_SERVER + `/posts/${id}/upload-cover`,
    {
      method: POST,
      headers: {
        ...AUTHENTICATION_HEADER(),
        ...JSON_HEADER,
      },
      body,
    },
  );

  if (!response.ok) {
    const data = (await response.json()) as IError;
    checkResponseStatus(response.status);
    throw FetchDataException(data.message);
  }

  revalidateTag(`/admin/posts/${id}`);
}
