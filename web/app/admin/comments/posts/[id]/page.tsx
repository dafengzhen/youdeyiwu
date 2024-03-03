import { type Metadata } from 'next';
import { notFound } from 'next/navigation';
import { isNum } from '@/app/common/tool';
import CommentReply from '@/app/admin/comments/posts/[id]/comment-reply';
import QueryDetailsPostAction from '@/app/actions/posts/query-details-post-action';
import LoginInfoUserAction from '@/app/actions/users/login-info-user-action';
import UpdateState from '@/app/admin/comments/posts/[id]/update-state';
import QueryCommentAction from '@/app/actions/comments/query-comment-action';
import QueryReplyAction from '@/app/actions/replies/query-reply-action';
import ErrorPage from '@/app/common/error-page';

export const metadata: Metadata = {
  title: 'Update Comment',
};

export default async function Page({
  params,
  searchParams,
}: {
  params: {
    id: string;
  };
  searchParams: {
    type?: 'state';
    cid?: string;
    rid?: string;
  };
}) {
  const id = params.id;
  if (!isNum(id)) {
    notFound();
  }

  if (searchParams.type === 'state') {
    const cid = searchParams.cid;
    const rid = searchParams.rid;

    if (!cid && !rid) {
      notFound();
    }
    if (cid && !isNum(cid)) {
      notFound();
    }
    if (rid && !isNum(rid)) {
      notFound();
    }

    const responses = await Promise.all([
      QueryCommentAction({ id: cid as string }),
      QueryReplyAction({
        id: rid!,
      }),
    ]);
    const commentResponse = responses[0];
    const replyResponse = responses[1];

    if (commentResponse.isError) {
      return <ErrorPage message={commentResponse.message} />;
    }

    if (replyResponse.isError) {
      return <ErrorPage message={replyResponse.message} />;
    }

    return (
      <UpdateState
        details={cid ? commentResponse.data : replyResponse.data}
        cid={cid ? parseInt(cid) : undefined}
        rid={rid ? parseInt(rid) : undefined}
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
