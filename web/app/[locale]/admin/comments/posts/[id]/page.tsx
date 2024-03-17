import { type Metadata } from 'next';
import { notFound } from 'next/navigation';
import { isNum } from '@/app/[locale]/common/tool';
import CommentReply from '@/app/[locale]/admin/comments/posts/[id]/comment-reply';
import QueryDetailsPostAction from '@/app/[locale]/actions/posts/query-details-post-action';
import LoginInfoUserAction from '@/app/[locale]/actions/users/login-info-user-action';
import UpdateState from '@/app/[locale]/admin/comments/posts/[id]/update-state';
import QueryCommentAction from '@/app/[locale]/actions/comments/query-comment-action';
import QueryReplyAction from '@/app/[locale]/actions/replies/query-reply-action';
import ErrorPage from '@/app/[locale]/common/error-page';
import queryString from 'query-string';

export const metadata: Metadata = {
  title: 'Update Comment',
};

export interface ISearchParamsAdminPostCommentPage {
  type?: 'state';
  cid?: string;
  rid?: string;
}

export default async function Page({
  params,
  searchParams,
}: {
  params: {
    id: string;
  };
  searchParams: ISearchParamsAdminPostCommentPage;
}) {
  const id = params.id;
  if (!isNum(id)) {
    notFound();
  }

  const _searchParams = parseSearchParams(searchParams);
  if (_searchParams.type === 'state') {
    const cid = _searchParams.cid;
    const rid = _searchParams.rid;

    if (typeof cid !== 'number' && typeof rid !== 'number') {
      notFound();
    }

    let commentResponse;
    let replyResponse;

    if (typeof cid === 'number') {
      commentResponse = await QueryCommentAction({ id: cid });
      if (commentResponse.isError) {
        return <ErrorPage message={commentResponse.message} />;
      }
    }

    if (typeof rid === 'number') {
      replyResponse = await QueryReplyAction({ id: rid });
      if (replyResponse.isError) {
        return <ErrorPage message={replyResponse.message} />;
      }
    }

    return (
      <UpdateState
        details={
          typeof cid === 'number'
            ? commentResponse?.data!
            : replyResponse?.data!
        }
        cid={typeof cid === 'number' ? cid : undefined}
        rid={typeof rid === 'number' ? rid : undefined}
      />
    );
  }

  const responses = await Promise.all([
    QueryDetailsPostAction({ id }),
    LoginInfoUserAction(),
  ]);
  const postResponse = responses[0];
  const currentUserResponse = responses[1];

  if (postResponse.isError) {
    return <ErrorPage message={postResponse.message} />;
  }

  if (currentUserResponse.isError) {
    return <ErrorPage message={currentUserResponse.message} />;
  }

  return (
    <CommentReply
      details={postResponse.data}
      currentUser={currentUserResponse.data}
    />
  );
}

const parseSearchParams = (searchParams: ISearchParamsAdminPostCommentPage) => {
  const { type, cid, rid } = searchParams;
  const params = {
    type,
    cid,
    rid,
  };

  const parse = queryString.parse(queryString.stringify(params), {
    parseNumbers: true,
  }) as Record<string, string | number>;
  return { ...parse };
};
