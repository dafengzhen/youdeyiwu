import { type Metadata } from 'next';
import Update from '@/app/admin/roles/[id]/update';
import Delete from '@/app/admin/roles/[id]/delete';
import { notFound } from 'next/navigation';
import { isNum } from '@/app/common/tool';
import UpdatePermissions from '@/app/admin/roles/[id]/update-permissions';
import QueryRoleAction from '@/app/actions/roles/query-role-action';
import ErrorPage from '@/app/common/error-page';

export const metadata: Metadata = {
  title: 'Update Role',
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

  const response = await QueryRoleAction({ id });
  if (response.isError) {
    return <ErrorPage message={response.message} />;
  }

  const type = searchParams.type;
  switch (type) {
    case 'del':
      return <Delete role={response.data} />;
    case 'permissions':
      return <UpdatePermissions role={response.data} />;
    default:
      return <Update role={response.data} />;
  }
}
