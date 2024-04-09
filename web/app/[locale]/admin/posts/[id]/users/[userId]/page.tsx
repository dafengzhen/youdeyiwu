import { type Metadata } from 'next';
import { notFound } from 'next/navigation';
import DisableCommentReply from '@/app/[locale]/admin/posts/[id]/users/disable-comment-reply';
import { isNum } from '@/app/[locale]/common/tool';
import QueryPostAction from '@/app/[locale]/actions/posts/query-post-action';
import ErrorPage from '@/app/[locale]/common/error-page';
import QueryUserRelationshipByIdPostAction from '@/app/[locale]/actions/posts/query-user-relationship-by-id-post-action';

export const metadata: Metadata = {
  title: 'Update Post',
};

export default async function Page({
  params,
  searchParams,
}: {
  params: {
    id: string;
    userId: string;
  };
  searchParams: {
    type?: 'updateDisableCommentReply';
  };
}) {
  const id = params.id;
  const userId = params.userId;
  if (!isNum(id) || !isNum(userId)) {
    notFound();
  }

  const responses = await Promise.all([
    QueryPostAction({ id }),
    QueryUserRelationshipByIdPostAction({
      id,
      userId,
    }),
  ]);

  const postResponse = responses[0];
  const userRelationshipResponse = responses[1];

  if (postResponse.isError) {
    return <ErrorPage message={postResponse.message} />;
  }

  if (userRelationshipResponse.isError) {
    return <ErrorPage message={userRelationshipResponse.message} />;
  }

  const type = searchParams.type;
  switch (type) {
    case 'updateDisableCommentReply':
      return (
        <DisableCommentReply
          post={postResponse.data}
          userRelationship={userRelationshipResponse.data}
        />
      );
    default:
      notFound();
  }
}
