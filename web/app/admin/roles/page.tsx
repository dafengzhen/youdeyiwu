import { type Metadata } from 'next';
import Roles from '@/app/admin/roles/roles';
import QueryAllRoleAction from '@/app/actions/roles/query-all-role-action';
import Create from '@/app/admin/roles/create';

export const metadata: Metadata = {
  title: 'roles - youdeyiwu',
  description: 'query all role page',
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
      return <Roles data={await QueryAllRoleAction()} />;
  }
}
