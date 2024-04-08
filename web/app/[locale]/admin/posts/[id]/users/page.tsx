import { type Metadata } from 'next';
import { notFound } from 'next/navigation';
import DisableCommentReply from '@/app/[locale]/admin/posts/[id]/users/disable-comment-reply';
import { isNum } from '@/app/[locale]/common/tool';
import QueryPostAction from '@/app/[locale]/actions/posts/query-post-action';
import ErrorPage from '@/app/[locale]/common/error-page';

export const metadata: Metadata = {
  title: 'Update Post',
};

export default async function Page({
  searchParams,
}: {
  searchParams: {
    postId: string;
    type?: 'disableCommentReply';
  };
}) {
  const postId = searchParams.postId;
  if (!isNum(postId)) {
    notFound();
  }

  const response = await QueryPostAction({ id: postId });
  if (response.isError) {
    return <ErrorPage message={response.message} />;
  }

  const type = searchParams.type;
  switch (type) {
    case 'disableCommentReply':
      return <DisableCommentReply post={response.data} />;
    default:
      notFound();
  }
}
