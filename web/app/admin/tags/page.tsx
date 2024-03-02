import { type Metadata } from 'next';
import Tags from '@/app/admin/tags/tags';
import QueryAllTagAction from '@/app/actions/tags/query-all-tag-action';
import Create from '@/app/admin/tags/create';
import ErrorPage from '@/app/common/error-page';

export const metadata: Metadata = {
  title: 'Tags',
};

export default async function Page({
  searchParams,
}: {
  searchParams: {
    type?: 'add';
  };
}) {
  const response = await QueryAllTagAction();
  if (response.isError) {
    return <ErrorPage message={response.message} />;
  }

  const type = searchParams.type;
  switch (type) {
    case 'add':
      return <Create />;
    default:
      return <Tags data={response.data} />;
  }
}
