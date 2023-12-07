import { type Metadata } from 'next';
import Create from '@/app/admin/section-groups/create';
import SectionGroups from '@/app/admin/section-groups/section-groups';
import QueryAllSectionGroupAction from '@/app/actions/section-groups/query-all-section-group-action';

export const metadata: Metadata = {
  title: 'section groups - youdeyiwu',
  description: 'query all section group page',
};

export default async function Page({
  searchParams,
}: {
  searchParams: {
    type?: 'add';
  };
}) {
  const type = searchParams.type;
  switch (type) {
    case 'add':
      return <Create />;
    default:
      return <SectionGroups data={await QueryAllSectionGroupAction()} />;
  }
}
