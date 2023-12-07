import { type Metadata } from 'next';
import Posts from '@/app/admin/posts/posts';
import QueryAllPostAction from '@/app/actions/posts/query-all-post-action';

export const metadata: Metadata = {
  title: 'posts - youdeyiwu',
  description: 'query all post page',
};

export default async function Page({ searchParams }: { searchParams: {} }) {
  return <Posts data={await QueryAllPostAction()} />;
}
