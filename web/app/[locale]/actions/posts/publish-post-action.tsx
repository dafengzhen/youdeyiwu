'use server';

import { type IError } from '@/app/[locale]/interfaces';
import { LOCATION, POST, PUT } from '@/app/[locale]/constants';
import { revalidateTag } from 'next/cache';
import {
  createErrorResponse,
  createRequest,
  createRequestUrl,
  createSuccessResponse,
} from '@/app/[locale]/common/response';

export interface IPublishPostActionVariables {
  name?: string;
  cover?: string;
  overview?: string;
  content?: string;
  contentLink?: string;
  tags?: string[];
  sectionId?: number;
  removeSection?: boolean;
}

export default async function PublishPostAction({
  id,
  variables,
}: {
  id?: number;
  variables?: IPublishPostActionVariables;
}) {
  try {
    const config: any = {};
    let path = '/posts';
    if (id) {
      config.cache = 'no-store';
      path = `/posts/${id}`;
    }

    const { url, str } = createRequestUrl(path);
    const response = await createRequest({
      url,
      options: {
        method: id ? PUT : POST,
        body: variables,
        ...config,
      },
    });

    if (!response.ok) {
      const data = (await response.json()) as IError;
      return createErrorResponse(data);
    }

    if (id) {
      revalidateTag(`/admin/posts/${id}`);
      return createSuccessResponse(null);
    } else {
      revalidateTag('/admin/posts');
      revalidateTag('/posts/select-all');
      return createSuccessResponse(response.headers.get(LOCATION) ?? '');
    }
  } catch (e) {
    return createErrorResponse(e);
  }
}
