import { type Metadata } from 'next';
import Update from '@/app/admin/menus/[id]/update';
import Delete from '@/app/admin/menus/[id]/delete';
import { notFound } from 'next/navigation';
import { isNum } from '@/app/common/tool';
import QueryMenuAction from '@/app/actions/menus/query-menu-action';
import UpdateRoles from '@/app/admin/menus/[id]/update-roles';
import ErrorPage from '@/app/common/error-page';

export const metadata: Metadata = {
  title: 'Update Menu',
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

  const response = await QueryMenuAction({ id });
  if (response.isError) {
    return <ErrorPage message={response.message} />;
  }

  const type = searchParams.type;
  switch (type) {
    case 'del':
      return <Delete menu={response.data} />;
    case 'roles':
      return <UpdateRoles menu={response.data} />;
    default:
      return <Update menu={response.data} />;
  }
}
