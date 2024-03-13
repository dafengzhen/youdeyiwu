import { type Metadata } from 'next';
import Delete from '@/app/[locale]/admin/users/[id]/delete';
import { notFound } from 'next/navigation';
import { isNum } from '@/app/[locale]/common/tool';
import UpdateStates from '@/app/[locale]/admin/users/[id]/update-states';
import UpdateRoles from '@/app/[locale]/admin/users/[id]/update-roles';
import QueryUserAction from '@/app/[locale]/actions/users/query-user-action';
import ErrorPage from '@/app/[locale]/common/error-page';

export const metadata: Metadata = {
  title: 'Update Tag',
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

  const response = await QueryUserAction({ id });
  if (response.isError) {
    return <ErrorPage message={response.message} />;
  }

  const type = searchParams.type;
  switch (type) {
    case 'del':
      return <Delete user={response.data} />;
    case 'states':
      return <UpdateStates user={response.data} />;
    case 'roles':
      return <UpdateRoles user={response.data} />;
    default:
      notFound();
  }
}
