import { type Metadata } from 'next';
import Update from '@/app/[locale]/admin/menus/[id]/update';
import Delete from '@/app/[locale]/admin/menus/[id]/delete';
import { notFound } from 'next/navigation';
import { isNum } from '@/app/[locale]/common/tool';
import QueryMenuAction from '@/app/[locale]/actions/menus/query-menu-action';
import UpdateRoles from '@/app/[locale]/admin/menus/[id]/update-roles';
import ErrorPage from '@/app/[locale]/common/error-page';

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
