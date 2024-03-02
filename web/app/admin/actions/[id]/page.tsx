import { type Metadata } from 'next';
import Update from '@/app/admin/actions/[id]/update';
import Delete from '@/app/admin/actions/[id]/delete';
import { notFound } from 'next/navigation';
import { isNum } from '@/app/common/server';
import QueryActionAction from '@/app/actions/actions/query-action-action';
import UpdateRoles from '@/app/admin/actions/[id]/update-roles';
import ErrorPage from '@/app/common/error-page';

export const metadata: Metadata = {
  title: 'Update Action',
};

export default async function Page({
  params,
  searchParams,
}: {
  params: {
    id: string;
  };
  searchParams: {
    type?: 'del' | 'role';
  };
}) {
  const id = params.id;
  if (!isNum(id)) {
    notFound();
  }

  const response = await QueryActionAction({ id });
  if (response.isError) {
    return <ErrorPage message={response.message} />;
  }

  const type = searchParams.type;
  switch (type) {
    case 'del':
      return <Delete action={response.data} />;
    case 'role':
      return <UpdateRoles action={response.data} />;
    default:
      return <Update action={response.data} />;
  }
}
