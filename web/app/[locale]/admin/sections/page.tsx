import { type Metadata } from 'next';
import Sections from '@/app/[locale]/admin/sections/sections';
import Create from '@/app/[locale]/admin/sections/create';
import QueryAllSectionAction from '@/app/[locale]/actions/sections/query-all-section-action';
import ErrorPage from '@/app/[locale]/common/error-page';

export const metadata: Metadata = {
  title: 'Sections',
};

export default async function Page({
  searchParams,
}: {
  searchParams: {
    type?: 'add';
    sKey?: string;
    sectionKey?: string;
  };
}) {
  const sectionKey = searchParams.sectionKey ?? searchParams.sKey;
  const response = await QueryAllSectionAction(
    sectionKey ? { sectionKey: sectionKey } : undefined,
  );
  if (response.isError) {
    return <ErrorPage message={response.message} />;
  }

  const type = searchParams.type;
  switch (type) {
    case 'add':
      return <Create />;
    default:
      return <Sections data={response.data} />;
  }
}
