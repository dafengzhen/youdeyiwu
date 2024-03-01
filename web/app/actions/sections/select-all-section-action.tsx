'use server';

import { type IError } from '@/app/interfaces';
import FetchDataException from '@/app/exception/fetch-data-exception';
import type { ISection } from '@/app/interfaces/sections';
import { AUTHENTICATION_HEADER } from '@/app/constants';
import { checkResponseStatus } from '@/app/common/server';
import queryString from 'query-string';

export default async function SelectAllSectionAction(variables?: {
  sectionKey?: string;
}) {
  const _queryParams = variables ?? {};
  const { url, str } = {
    url: queryString.stringifyUrl({
      url: process.env.API_SERVER + '/sections/select-all',
      query: _queryParams,
    }),
    str: queryString.stringify(_queryParams),
  };

  const response = await fetch(url, {
    headers: AUTHENTICATION_HEADER(),
    next: {
      tags: ['/sections/select-all', str],
    },
  });

  const data = (await response.json()) as ISection[] | IError;
  if (!response.ok) {
    checkResponseStatus(response.status);
    throw FetchDataException((data as IError).message);
  }

  return data as ISection[];
}
