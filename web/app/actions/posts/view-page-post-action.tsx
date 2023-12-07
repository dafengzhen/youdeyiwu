'use server';

import { type IError } from '@/app/interfaces';
import FetchDataException from '@/app/exception/fetch-data-exception';
import { AUTHENTICATION_HEADER, JSON_HEADER, POST } from '@/app/constants';
import { checkResponseStatus, getXRealIp } from '@/app/common/server';

export default async function ViewPagePostAction(variables: {
  id: number | string;
}) {
  const id = getXRealIp();
  if (!id) {
    console.info('The IP address is empty, skip recording post page views');
    return;
  }

  const response = await fetch(
    process.env.API_SERVER + `/posts/${variables.id}/view-page?ip=${id}`,
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
}
