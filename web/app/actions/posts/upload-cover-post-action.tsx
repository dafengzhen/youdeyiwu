'use server';

import { type IError } from '@/app/interfaces';
import FetchDataException from '@/app/exception/fetch-data-exception';
import { AUTHENTICATION_HEADER, POST } from '@/app/constants';
import { revalidateTag } from 'next/cache';
import { checkResponseStatus } from '@/app/common/server';

export interface IUploadCoverPostActionVariables {
  formData: FormData;
}

export default async function UploadCoverPostAction({
  id,
  variables,
}: {
  id: number;
  variables: IUploadCoverPostActionVariables;
}) {
  const response = await fetch(
    process.env.API_SERVER + `/posts/${id}/upload-cover`,
    {
      method: POST,
      headers: AUTHENTICATION_HEADER(),
      body: variables.formData,
    },
  );

  if (!response.ok) {
    const data = (await response.json()) as IError;
    checkResponseStatus(response.status);
    throw FetchDataException(data.message);
  }

  revalidateTag(`/admin/posts/${id}`);
}
