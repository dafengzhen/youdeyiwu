import { type Metadata } from 'next';
import Posts from '@/app/admin/posts/review-queues/posts';
import QueryAllPostReviewQueuesAction from '@/app/actions/posts/review-queues/query-all-post-review-queues-action';

export const metadata: Metadata = {
  title: 'Post Review Queues',
};

export default async function Page({ searchParams }: { searchParams: {} }) {
  return <Posts data={await QueryAllPostReviewQueuesAction()} />;
}
