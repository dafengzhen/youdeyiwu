import { type Metadata } from 'next';
import Update from '@/app/admin/submenus/[id]/update';
import Delete from '@/app/admin/submenus/[id]/delete';
import { notFound } from 'next/navigation';
import { isNum } from '@/app/common/server';
import QuerySubmenuAction from '@/app/actions/submenus/query-submenu-action';
import UpdateRoles from '@/app/admin/submenus/[id]/update-roles';
import ErrorPage from '@/app/common/error-page';

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

  const response = await QuerySubmenuAction({ id });
  if (response.isError) {
    return <ErrorPage message={response.message} />;
  }

  const type = searchParams.type;
  switch (type) {
    case 'del':
      return <Delete submenu={response.data} />;
    case 'roles':
      return <UpdateRoles submenu={response.data} />;
    default:
      return <Update submenu={response.data} />;
  }
}
