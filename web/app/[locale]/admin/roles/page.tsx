import { type Metadata } from 'next';
import Roles from '@/app/[locale]/admin/roles/roles';
import QueryAllRoleAction from '@/app/[locale]/actions/roles/query-all-role-action';
import Create from '@/app/[locale]/admin/roles/create';
import ErrorPage from '@/app/[locale]/common/error-page';

export const metadata: Metadata = {
  title: 'Roles',
};

export default async function Page({
  searchParams,
}: {
  searchParams: {
    type?: 'add';
  };
}) {
  const response = await QueryAllRoleAction();
  if (response.isError) {
    return <ErrorPage message={response.message} />;
  }

  const type = searchParams.type;
  switch (type) {
    case 'add':
      return <Create />;
    default:
      return <Roles data={response.data} />;
  }
}
