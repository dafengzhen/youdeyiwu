import { type Metadata } from 'next';
import Update from '@/app/admin/roles/[id]/update';
import Delete from '@/app/admin/roles/[id]/delete';
import { notFound } from 'next/navigation';
import { isNum } from '@/app/common/server';
import UpdatePermissions from '@/app/admin/roles/[id]/update-permissions';
import QueryRoleAction from '@/app/actions/roles/query-role-action';

export const metadata: Metadata = {
  title: 'update role - youdeyiwu',
  description: 'update role page',
};

export default async function Page({
  params,
  searchParams,
}: {
  params: {
    id: string;
  };
  searchParams: {
    type?: 'del' | 'permissions';
  };
}) {
  const id = params.id;
  if (!isNum(id)) {
    notFound();
  }

  const role = await QueryRoleAction({ id });
  const type = searchParams.type;
  switch (type) {
    case 'del':
      return <Delete role={role} />;
    case 'permissions':
      return <UpdatePermissions role={role} />;
    default:
      return <Update role={role} />;
  }
}
