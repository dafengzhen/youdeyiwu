import { type Metadata } from 'next';
import Update from '@/app/admin/tags/[id]/update';
import Delete from '@/app/admin/tags/[id]/delete';
import { notFound } from 'next/navigation';
import { isNum } from '@/app/common/tool';
import QueryTagAction from '@/app/actions/tags/query-tag-action';
import ErrorPage from '@/app/common/error-page';

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
    type?: 'del';
  };
}) {
  const id = params.id;
  if (!isNum(id)) {
    notFound();
  }

  const response = await QueryTagAction({ id });
  if (response.isError) {
    return <ErrorPage message={response.message} />;
  }

  const type = searchParams.type;
  switch (type) {
    case 'del':
      return <Delete tag={response.data} />;
    default:
      return <Update tag={response.data} />;
  }
}
