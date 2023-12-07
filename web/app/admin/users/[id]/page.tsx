import { type Metadata } from 'next';
import Delete from '@/app/admin/users/[id]/delete';
import { notFound } from 'next/navigation';
import { isNum } from '@/app/common/server';
import UpdateStates from '@/app/admin/users/[id]/update-states';
import UpdateRoles from '@/app/admin/users/[id]/update-roles';
import QueryUserAction from '@/app/actions/users/query-user-action';

export const metadata: Metadata = {
  title: 'update tag - youdeyiwu',
  description: 'update tag page',
};

export default async function Page({
  params,
  searchParams,
}: {
  params: {
    id: string;
  };
  searchParams: {
    type?: 'del' | 'states' | 'roles';
  };
}) {
  const id = params.id;
  if (!isNum(id)) {
    notFound();
  }

  const user = await QueryUserAction({ id });
  const type = searchParams.type;
  switch (type) {
    case 'del':
      return <Delete user={user} />;
    case 'states':
      return <UpdateStates user={user} />;
    case 'roles':
      return <UpdateRoles user={user} />;
    default:
      notFound();
  }
}
