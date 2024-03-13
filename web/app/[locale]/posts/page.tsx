import { type Metadata } from 'next';
import Posts from '@/app/[locale]/posts/posts';
import SelectAllPostAction from '@/app/[locale]/actions/posts/select-all-post-action';
import ErrorPage from '@/app/[locale]/common/error-page';

export const metadata: Metadata = {
  title: 'Articles',
};

export default async function Page() {
  const response = await SelectAllPostAction();
  if (response.isError) {
    return <ErrorPage message={response.message} />;
  }

  return <Posts data={response.data} />;
}
