'use server';

import { type IError } from '@/app/interfaces';
import FetchDataException from '@/app/exception/fetch-data-exception';
import type { ISection } from '@/app/interfaces/sections';
import { AUTHENTICATION_HEADER } from '@/app/constants';
import { checkResponseStatus } from '@/app/common/server';

export default async function SelectAllSectionAction(variables?: {
  sectionKey?: string;
}) {
  const _variables = variables ?? {};
  let url = process.env.API_SERVER + '/sections/select-all';
  if (_variables.sectionKey) {
    url =
      process.env.API_SERVER +
      `/sections/select-all?sectionKey=${_variables.sectionKey}`;
  }

  const response = await fetch(url, {
    headers: AUTHENTICATION_HEADER(),
    next: {
      tags: [
        _variables.sectionKey
          ? `/sections/select-all?sectionKey=${_variables.sectionKey}`
          : '/sections/select-all',
      ],
    },
  });

  const data = (await response.json()) as ISection[] | IError;
  if (!response.ok) {
    checkResponseStatus(response.status);
    throw FetchDataException((data as IError).message);
  }

  return data as ISection[];
}
