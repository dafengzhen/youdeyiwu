'use server';

import { type IError } from '@/app/interfaces';
import FetchDataException from '@/app/exception/fetch-data-exception';
import { AUTHENTICATION_HEADER, JSON_HEADER, POST } from '@/app/constants';
import { revalidateTag } from 'next/cache';
import { checkResponseStatus } from '@/app/common/server';

export interface ICreateGlobalMessageActionVariables {
  name: string;
  overview: string;
  link?: string;
  content?: Record<string, any>;
  sort: number;
}

export default async function CreateGlobalMessageAction(
  variables: ICreateGlobalMessageActionVariables,
) {
  const response = await fetch(
    process.env.API_SERVER + '/messages/global-messages',
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

  revalidateTag('/admin/messages/global-messages');
}
