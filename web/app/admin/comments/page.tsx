import { type Metadata } from 'next';
import Posts from '@/app/admin/comments/posts';
import QueryAllPostAction from '@/app/actions/posts/query-all-post-action';

export const metadata: Metadata = {
  title: 'Comments',
};

export default async function Page() {
  return <Posts data={await QueryAllPostAction()} />;
}
