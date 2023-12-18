'use server';

import { type IError } from '@/app/interfaces';
import FetchDataException from '@/app/exception/fetch-data-exception';
import { AUTHENTICATION_HEADER } from '@/app/constants';
import { checkResponseStatus } from '@/app/common/server';
import { IGlobalMessage } from '@/app/interfaces/messages';

export default async function QueryGlobalMessageAction(variables: {
  id: number | string;
}) {
  const response = await fetch(
    process.env.API_SERVER + `/messages/global-messages/${variables.id}`,
    {
      headers: AUTHENTICATION_HEADER(),
      next: {
        tags: [`/admin/messages/global-messages/${variables.id}`],
      },
    },
  );

  const data = (await response.json()) as IGlobalMessage | IError;
  if (!response.ok) {
    checkResponseStatus(response.status);
    throw FetchDataException((data as IError).message);
  }

  return data as IGlobalMessage;
}
