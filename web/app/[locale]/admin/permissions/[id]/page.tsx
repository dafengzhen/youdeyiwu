import { type Metadata } from 'next';
import Update from '@/app/[locale]/admin/permissions/[id]/update';
import Delete from '@/app/[locale]/admin/permissions/[id]/delete';
import { notFound } from 'next/navigation';
import { isNum } from '@/app/[locale]/common/tool';
import UpdateRoles from '@/app/[locale]/admin/permissions/[id]/update-roles';
import QueryPermissionAction from '@/app/[locale]/actions/permissions/query-permission-action';
import ErrorPage from '@/app/[locale]/common/error-page';

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
