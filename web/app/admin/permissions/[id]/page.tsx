import { type Metadata } from 'next';
import Update from '@/app/admin/permissions/[id]/update';
import Delete from '@/app/admin/permissions/[id]/delete';
import { notFound } from 'next/navigation';
import { isNum } from '@/app/common/server';
import UpdateRoles from '@/app/admin/permissions/[id]/update-roles';
import QueryPermissionAction from '@/app/actions/permissions/query-permission-action';

export const metadata: Metadata = {
  title: 'Update Permission',
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

  const permission = await QueryPermissionAction({ id });
  const type = searchParams.type;
  switch (type) {
    case 'del':
      return <Delete permission={permission} />;
    case 'roles':
      return <UpdateRoles permission={permission} />;
    default:
      return <Update permission={permission} />;
  }
}
