import { type Metadata } from 'next';
import Create from '@/app/admin/menus/create';
import Menus from '@/app/admin/menus/menus';
import QueryAllMenuAction from '@/app/actions/menus/query-all-menu-action';

export const metadata: Metadata = {
  title: 'menus - youdeyiwu',
  description: 'query all menu page',
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
      return <Menus data={await QueryAllMenuAction()} />;
  }
}
