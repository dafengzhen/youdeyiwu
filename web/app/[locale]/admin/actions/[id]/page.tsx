import { type Metadata } from 'next';
import Update from '@/app/[locale]/admin/actions/[id]/update';
import Delete from '@/app/[locale]/admin/actions/[id]/delete';
import { notFound } from 'next/navigation';
import QueryActionAction from '@/app/[locale]/actions/actions/query-action-action';
import UpdateRoles from '@/app/[locale]/admin/actions/[id]/update-roles';
import ErrorPage from '@/app/[locale]/common/error-page';
import { isNum } from '@/app/[locale]/common/tool';

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
