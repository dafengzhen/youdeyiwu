import { type Metadata } from 'next';
import Update from '@/app/admin/tag-groups/[id]/update';
import Delete from '@/app/admin/tag-groups/[id]/delete';
import { notFound } from 'next/navigation';
import { isNum } from '@/app/common/server';
import QueryTagGroupAction from '@/app/actions/tag-groups/query-tag-group-action';
import UpdateTags from '@/app/admin/tag-groups/[id]/update-tags';
import ErrorPage from '@/app/common/error-page';

export const metadata: Metadata = {
  title: 'Update Tag Group',
};

export default async function Page({
  params,
  searchParams,
}: {
  params: {
    id: string;
  };
  searchParams: {
    type?: 'del' | 'tags';
  };
}) {
  const id = params.id;
  if (!isNum(id)) {
    notFound();
  }

  const response = await QueryTagGroupAction({ id });
  if (response.isError) {
    return <ErrorPage message={response.message} />;
  }

  const type = searchParams.type;
  switch (type) {
    case 'del':
      return <Delete tagGroup={response.data} />;
    case 'tags':
      return <UpdateTags tagGroup={response.data} />;
    default:
      return <Update tagGroup={response.data} />;
  }
}
