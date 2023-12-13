import { type Metadata } from 'next';
import Update from '@/app/admin/menus/[id]/update';
import Delete from '@/app/admin/menus/[id]/delete';
import { notFound } from 'next/navigation';
import { isNum } from '@/app/common/server';
import QueryMenuAction from '@/app/actions/menus/query-menu-action';

export const metadata: Metadata = {
  title: 'update menu - youdeyiwu',
  description: 'update menu page',
};

export default async function Page({
  params,
  searchParams,
}: {
  params: {
    id: string;
  };
  searchParams: {
    type?: 'del';
  };
}) {
  const id = params.id;
  if (!isNum(id)) {
    notFound();
  }

  const menu = await QueryMenuAction({ id });
  const type = searchParams.type;
  switch (type) {
    case 'del':
      return <Delete menu={menu} />;
    default:
      return <Update menu={menu} />;
  }
}
