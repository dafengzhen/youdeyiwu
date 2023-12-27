import { type Metadata } from 'next';
import { notFound } from 'next/navigation';
import { errorContent, isNum } from '@/app/common/server';
import CommentReply from '@/app/admin/comments/posts/[id]/comment-reply';
import QueryDetailsPostAction from '@/app/actions/posts/query-details-post-action';
import LoginInfoUserAction from '@/app/actions/users/login-info-user-action';
import UpdateState from '@/app/admin/comments/posts/[id]/update-state';
import QueryCommentAction from '@/app/actions/comments/query-comment-action';
import QueryReplyAction from '@/app/actions/replies/query-reply-action';
import ClientErrorHandler from '@/app/common/client-error-handler';

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

    const details = cid
      ? await QueryCommentAction({ id: cid })
      : await QueryReplyAction({
          id: rid!,
        });
    return (
      <UpdateState
        details={details}
        cid={cid ? parseInt(cid) : undefined}
        rid={rid ? parseInt(rid) : undefined}
      />
    );
  }

  try {
    return (
      <CommentReply
        details={await QueryDetailsPostAction({ id })}
        currentUser={await LoginInfoUserAction()}
      />
    );
  } catch (e) {
    return <ClientErrorHandler message={errorContent(e)} />;
  }
}
