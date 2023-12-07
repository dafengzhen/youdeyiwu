import { type Metadata } from 'next';
import Update from '@/app/admin/section-groups/[id]/update';
import Delete from '@/app/admin/section-groups/[id]/delete';
import { notFound } from 'next/navigation';
import { isNum } from '@/app/common/server';
import QuerySectionGroupAction from '@/app/actions/section-groups/query-section-group-action';
import UpdateSections from '@/app/admin/section-groups/[id]/update-sections';

export const metadata: Metadata = {
  title: 'update section group - youdeyiwu',
  description: 'update section group page',
};

export default async function Page({
  params,
  searchParams,
}: {
  params: {
    id: string;
  };
  searchParams: {
    type?: 'del' | 'sections';
  };
}) {
  const id = params.id;
  if (!isNum(id)) {
    notFound();
  }

  const sectionGroup = await QuerySectionGroupAction({ id });
  const type = searchParams.type;
  switch (type) {
    case 'del':
      return <Delete sectionGroup={sectionGroup} />;
    case 'sections':
      return <UpdateSections sectionGroup={sectionGroup} />;
    default:
      return <Update sectionGroup={sectionGroup} />;
  }
}
