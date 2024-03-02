import { type Metadata } from 'next';
import Create from '@/app/admin/tag-groups/create';
import TagGroups from '@/app/admin/tag-groups/tag-groups';
import QueryAllTagGroupAction from '@/app/actions/tag-groups/query-all-tag-group-action';
import ErrorPage from '@/app/common/error-page';

export const metadata: Metadata = {
  title: 'Tag Groups',
};

export default async function Page({
  searchParams,
}: {
  searchParams: {
    type?: 'add';
  };
}) {
  const response = await QueryAllTagGroupAction();
  if (response.isError) {
    return <ErrorPage message={response.message} />;
  }

  const type = searchParams.type;
  switch (type) {
    case 'add':
      return <Create />;
    default:
      return <TagGroups data={response.data} />;
  }
}
