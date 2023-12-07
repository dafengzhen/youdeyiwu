'use server';

import { type IError } from '@/app/interfaces';
import FetchDataException from '@/app/exception/fetch-data-exception';
import {
  AUTHENTICATION_HEADER,
  JSON_HEADER,
  LOCATION,
  POST,
  PUT,
} from '@/app/constants';
import { revalidateTag } from 'next/cache';
import { checkResponseStatus } from '@/app/common/server';

export interface IPublishPostActionVariables {
  name?: string;
  cover?: string;
  overview?: string;
  content?: string;
  contentLink?: string;
  tags?: string[];
  sectionId?: string | 'none';
}

export default async function PublishPostAction({
  id,
  variables,
}: {
  id?: number;
  variables?: IPublishPostActionVariables;
}) {
  const config: any = {};
  let url = process.env.API_SERVER + '/posts';
  if (id) {
    config.cache = 'no-store';
    url = process.env.API_SERVER + `/posts/${id}`;
  }

  const response = await fetch(url, {
    method: id ? PUT : POST,
    headers: {
      ...AUTHENTICATION_HEADER(),
      ...JSON_HEADER,
    },
    body: JSON.stringify(variables),
    ...config,
  });

  if (!response.ok) {
    const data = (await response.json()) as IError;
    checkResponseStatus(response.status);
    throw FetchDataException(data.message);
  }

  if (id) {
    revalidateTag(`/admin/posts/${id}`);
  } else {
    revalidateTag('/admin/posts');
    revalidateTag('/posts/select-all');
    return response.headers.get(LOCATION) ?? '';
  }
}
