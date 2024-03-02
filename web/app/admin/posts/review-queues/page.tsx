import { type Metadata } from 'next';
import Posts from '@/app/admin/posts/review-queues/posts';
import QueryAllPostReviewQueuesAction from '@/app/actions/posts/review-queues/query-all-post-review-queues-action';
import ErrorPage from '@/app/common/error-page';

export const metadata: Metadata = {
  title: 'Post Review Queues',
};

export default async function Page({ searchParams }: { searchParams: {} }) {
  const response = await QueryAllPostReviewQueuesAction();
  if (response.isError) {
    return <ErrorPage message={response.message} />;
  }

  return <Posts data={response.data} />;
}
