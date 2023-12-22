'use server';

import { type IError } from '@/app/interfaces';
import FetchDataException from '@/app/exception/fetch-data-exception';
import { AUTHENTICATION_HEADER, POST } from '@/app/constants';
import { revalidateTag } from 'next/cache';
import { checkResponseStatus } from '@/app/common/server';

export interface IUploadCoverSectionActionVariables {
  formData: FormData;
}

export default async function UploadCoverSectionAction({
  id,
  variables,
}: {
  id: number;
  variables: IUploadCoverSectionActionVariables;
}) {
  const response = await fetch(
    process.env.API_SERVER + `/sections/${id}/upload-cover`,
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

  revalidateTag(`/admin/sections/${id}`);
}
