import { type Metadata } from 'next';
import Create from '@/app/admin/tag-groups/create';
import TagGroups from '@/app/admin/tag-groups/tag-groups';
import QueryAllTagGroupAction from '@/app/actions/tag-groups/query-all-tag-group-action';

export const metadata: Metadata = {
  title: 'tag groups - youdeyiwu',
  description: 'query all tag group page',
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
      return <TagGroups data={await QueryAllTagGroupAction()} />;
  }
}
