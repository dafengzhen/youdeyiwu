'use server';

import { type IError, IPage, TQueryParams } from '@/app/interfaces';
import FetchDataException from '@/app/exception/fetch-data-exception';
import { ICommentReply } from '@/app/interfaces/posts';
import { checkResponseStatus } from '@/app/common/server';
import { AUTHENTICATION_HEADER } from '@/app/constants';
import queryString from 'query-string';

export default async function CommentReplyPostAction({
  id,
  queryParams,
}: {
  id: string | number;
  queryParams?: TQueryParams;
}) {
  const _queryParams = queryParams ?? {};
  const { url, str } = {
    url: queryString.stringifyUrl({
      url: process.env.API_SERVER + `/posts/${id}/comment-reply`,
      query: _queryParams,
    }),
    str: queryString.stringify(_queryParams),
  };

  const response = await fetch(url, {
    headers: AUTHENTICATION_HEADER(),
    next: {
      tags: [`/posts/${id}/comment-reply`, str],
    },
  });

  const data = (await response.json()) as IPage<ICommentReply[]> | IError;
  if (!response.ok) {
    checkResponseStatus(response.status);
    throw FetchDataException((data as IError).message);
  }

  return data as IPage<ICommentReply[]>;
}
