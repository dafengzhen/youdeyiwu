import { type Metadata } from 'next';
import Sections from '@/app/admin/sections/sections';
import Create from '@/app/admin/sections/create';
import QueryAllSectionAction from '@/app/actions/sections/query-all-section-action';

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
  const type = searchParams.type;
  const sectionKey = searchParams.sectionKey ?? searchParams.sKey;
  switch (type) {
    case 'add':
      return <Create />;
    default:
      return (
        <Sections
          data={await QueryAllSectionAction(
            sectionKey ? { sectionKey: sectionKey } : undefined,
          )}
        />
      );
  }
}
