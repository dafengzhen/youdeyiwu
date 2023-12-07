import { type Metadata } from 'next';
import Update from '@/app/admin/tags/[id]/update';
import Delete from '@/app/admin/tags/[id]/delete';
import { notFound } from 'next/navigation';
import { isNum } from '@/app/common/server';
import QueryTagAction from '@/app/actions/tags/query-tag-action';

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
    type?: 'del';
  };
}) {
  const id = params.id;
  if (!isNum(id)) {
    notFound();
  }

  const tag = await QueryTagAction({ id });
  const type = searchParams.type;
  switch (type) {
    case 'del':
      return <Delete tag={tag} />;
    default:
      return <Update tag={tag} />;
  }
}
