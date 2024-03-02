import { type Metadata } from 'next';
import Posts from '@/app/admin/comments/posts';
import QueryAllPostAction from '@/app/actions/posts/query-all-post-action';
import ErrorPage from '@/app/common/error-page';

export const metadata: Metadata = {
  title: 'Comments',
};

export default async function Page() {
  const response = await QueryAllPostAction();
  if (response.isError) {
    return <ErrorPage message={response.message} />;
  }

  return <Posts data={response.data} />;
}
