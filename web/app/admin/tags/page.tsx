import { type Metadata } from 'next';
import Tags from '@/app/admin/tags/tags';
import QueryAllTagAction from '@/app/actions/tags/query-all-tag-action';
import Create from '@/app/admin/tags/create';

export const metadata: Metadata = {
  title: 'tags - youdeyiwu',
  description: 'query all tag page',
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
      return <Tags data={await QueryAllTagAction()} />;
  }
}
