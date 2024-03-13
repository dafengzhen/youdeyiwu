import { type Metadata } from 'next';
import Create from '@/app/[locale]/admin/section-groups/create';
import SectionGroups from '@/app/[locale]/admin/section-groups/section-groups';
import QueryAllSectionGroupAction from '@/app/[locale]/actions/section-groups/query-all-section-group-action';
import ErrorPage from '@/app/[locale]/common/error-page';

export const metadata: Metadata = {
  title: 'Section Groups',
};

export default async function Page({
  searchParams,
}: {
  searchParams: {
    type?: 'add';
  };
}) {
  const response = await QueryAllSectionGroupAction();
  if (response.isError) {
    return <ErrorPage message={response.message} />;
  }

  const type = searchParams.type;
  switch (type) {
    case 'add':
      return <Create />;
    default:
      return <SectionGroups data={response.data} />;
  }
}
