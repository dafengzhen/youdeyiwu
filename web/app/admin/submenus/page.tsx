import { type Metadata } from 'next';
import Create from '@/app/admin/submenus/create';
import Submenus from '@/app/admin/submenus/submenus';
import QueryAllSubmenuAction from '@/app/actions/submenus/query-all-submenu-action';

export const metadata: Metadata = {
  title: 'submenus - youdeyiwu',
  description: 'query all submenu page',
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
      return <Submenus data={await QueryAllSubmenuAction()} />;
  }
}
