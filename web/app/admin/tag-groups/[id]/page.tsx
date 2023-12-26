import { type Metadata } from 'next';
import Update from '@/app/admin/tag-groups/[id]/update';
import Delete from '@/app/admin/tag-groups/[id]/delete';
import { notFound } from 'next/navigation';
import { isNum } from '@/app/common/server';
import QueryTagGroupAction from '@/app/actions/tag-groups/query-tag-group-action';
import UpdateTags from '@/app/admin/tag-groups/[id]/update-tags';

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

  const tagGroup = await QueryTagGroupAction({ id });
  const type = searchParams.type;
  switch (type) {
    case 'del':
      return <Delete tagGroup={tagGroup} />;
    case 'tags':
      return <UpdateTags tagGroup={tagGroup} />;
    default:
      return <Update tagGroup={tagGroup} />;
  }
}
