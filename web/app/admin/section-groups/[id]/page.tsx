import { type Metadata } from 'next';
import Update from '@/app/admin/section-groups/[id]/update';
import Delete from '@/app/admin/section-groups/[id]/delete';
import { notFound } from 'next/navigation';
import { isNum } from '@/app/common/server';
import QuerySectionGroupAction from '@/app/actions/section-groups/query-section-group-action';
import UpdateSections from '@/app/admin/section-groups/[id]/update-sections';
import ErrorPage from '@/app/common/error-page';

export const metadata: Metadata = {
  title: 'Update Section Group',
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

  const response = await QuerySectionGroupAction({ id });
  if (response.isError) {
    return <ErrorPage message={response.message} />;
  }

  const type = searchParams.type;
  switch (type) {
    case 'del':
      return <Delete sectionGroup={response.data} />;
    case 'sections':
      return <UpdateSections sectionGroup={response.data} />;
    default:
      return <Update sectionGroup={response.data} />;
  }
}
