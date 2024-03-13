import { type Metadata } from 'next';
import Update from '@/app/[locale]/admin/section-groups/[id]/update';
import Delete from '@/app/[locale]/admin/section-groups/[id]/delete';
import { notFound } from 'next/navigation';
import { isNum } from '@/app/[locale]/common/tool';
import QuerySectionGroupAction from '@/app/[locale]/actions/section-groups/query-section-group-action';
import UpdateSections from '@/app/[locale]/admin/section-groups/[id]/update-sections';
import ErrorPage from '@/app/[locale]/common/error-page';

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
