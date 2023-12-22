import { NextRequest } from 'next/server';
import { AUTHENTICATION_HEADER } from '@/app/constants';
import type { IError } from '@/app/interfaces';
import { checkResponseStatus } from '@/app/common/server';
import FetchDataException from '@/app/exception/fetch-data-exception';

export async function GET(
  request: NextRequest,
  context: { params: { id: string } },
) {
  const id = context.params.id;
  const response = await fetch(
    process.env.API_SERVER + `/sections/${id}/cover`,
    {
      headers: AUTHENTICATION_HEADER(),
    },
  );

  if (!response.ok) {
    const data = (await response.json()) as IError;
    checkResponseStatus(response.status);
    throw FetchDataException(data.message);
  }

  return response;
}
