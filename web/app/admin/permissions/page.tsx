import { type Metadata } from 'next';
import Permissions from '@/app/admin/permissions/permissions';
import Create from '@/app/admin/permissions/create';
import QueryAllPermissionAction from '@/app/actions/permissions/query-all-permission-action';
import ErrorPage from '@/app/common/error-page';

export const metadata: Metadata = {
  title: 'Permissions',
};

export default async function Page({
  searchParams,
}: {
  searchParams: {
    type?: 'add';
  };
}) {
  const response = await QueryAllPermissionAction();
  if (response.isError) {
    return <ErrorPage message={response.message} />;
  }

  const type = searchParams.type;
  switch (type) {
    case 'add':
      return <Create />;
    default:
      return <Permissions data={response.data} />;
  }
}
