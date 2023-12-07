'use server';

import { type IError, IPage, TQueryParams } from '@/app/interfaces';
import FetchDataException from '@/app/exception/fetch-data-exception';
import { ICommentReply } from '@/app/interfaces/posts';
import { checkResponseStatus, getQueryParams } from '@/app/common/server';
import { AUTHENTICATION_HEADER } from '@/app/constants';

export default async function CommentReplyPostAction({
  id,
  queryParams,
}: {
  id: string | number;
  queryParams?: TQueryParams;
}) {
  let url = process.env.API_SERVER + `/posts/${id}/comment-reply`;
  let params = '';
  if (queryParams) {
    params = getQueryParams(queryParams);
    url = url + '?' + params;
  }

  const response = await fetch(url, {
    headers: AUTHENTICATION_HEADER(),
    next: {
      tags: [`/posts/${id}/comment-reply` + params ? '?' + params : ''],
    },
  });

  const data = (await response.json()) as IPage<ICommentReply[]> | IError;
  if (!response.ok) {
    checkResponseStatus(response.status);
    throw FetchDataException((data as IError).message);
  }

  return data as IPage<ICommentReply[]>;
}
