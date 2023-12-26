import { type Metadata } from 'next';
import Update from '@/app/admin/submenus/[id]/update';
import Delete from '@/app/admin/submenus/[id]/delete';
import { notFound } from 'next/navigation';
import { isNum } from '@/app/common/server';
import QuerySubmenuAction from '@/app/actions/submenus/query-submenu-action';
import UpdateRoles from '@/app/admin/submenus/[id]/update-roles';

export const metadata: Metadata = {
  title: 'Update Submenu',
};

export default async function Page({
  params,
  searchParams,
}: {
  params: {
    id: string;
  };
  searchParams: {
    type?: 'del' | 'roles';
  };
}) {
  const id = params.id;
  if (!isNum(id)) {
    notFound();
  }

  const submenu = await QuerySubmenuAction({ id });
  const type = searchParams.type;
  switch (type) {
    case 'del':
      return <Delete submenu={submenu} />;
    case 'roles':
      return <UpdateRoles submenu={submenu} />;
    default:
      return <Update submenu={submenu} />;
  }
}
