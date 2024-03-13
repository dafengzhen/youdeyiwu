import { type Metadata } from 'next';
import Tags from '@/app/[locale]/admin/tags/tags';
import QueryAllTagAction from '@/app/[locale]/actions/tags/query-all-tag-action';
import Create from '@/app/[locale]/admin/tags/create';
import ErrorPage from '@/app/[locale]/common/error-page';

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
