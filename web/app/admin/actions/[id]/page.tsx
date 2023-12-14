import { type Metadata } from 'next';
import Update from '@/app/admin/actions/[id]/update';
import Delete from '@/app/admin/actions/[id]/delete';
import { notFound } from 'next/navigation';
import { isNum } from '@/app/common/server';
import QueryActionAction from '@/app/actions/actions/query-action-action';
import UpdateRole from '@/app/admin/actions/[id]/update-role';

export const metadata: Metadata = {
  title: 'update action - youdeyiwu',
  description: 'update action page',
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

  const action = await QueryActionAction({ id });
  const type = searchParams.type;
  switch (type) {
    case 'del':
      return <Delete action={action} />;
    case 'role':
      return <UpdateRole action={action} />;
    default:
      return <Update action={action} />;
  }
}
