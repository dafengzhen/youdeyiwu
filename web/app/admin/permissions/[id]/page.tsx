import { type Metadata } from 'next';
import Update from '@/app/admin/permissions/[id]/update';
import Delete from '@/app/admin/permissions/[id]/delete';
import { notFound } from 'next/navigation';
import { isNum } from '@/app/common/tool';
import UpdateRoles from '@/app/admin/permissions/[id]/update-roles';
import QueryPermissionAction from '@/app/actions/permissions/query-permission-action';
import ErrorPage from '@/app/common/error-page';

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

  const response = await QueryPermissionAction({ id });
  if (response.isError) {
    return <ErrorPage message={response.message} />;
  }

  const type = searchParams.type;
  switch (type) {
    case 'del':
      return <Delete permission={response.data} />;
    case 'roles':
      return <UpdateRoles permission={response.data} />;
    default:
      return <Update permission={response.data} />;
  }
}
