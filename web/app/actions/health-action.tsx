'use server';

import type { IError, IHealth } from '@/app/interfaces';
import FetchDataException from '@/app/exception/fetch-data-exception';
import { AUTHENTICATION_HEADER } from '@/app/constants';

export default async function HealthAction() {
  const response = await fetch(process.env.API_SERVER + '/actuator/health', {
    headers: AUTHENTICATION_HEADER(),
  });

  const data = (await response.json()) as IHealth | IError;
  if (!response.ok) {
    console.error(data);
    throw FetchDataException((data as IError).message);
  }

  return data as IHealth;
}
