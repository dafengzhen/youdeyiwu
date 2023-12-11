import { type Metadata } from 'next';
import Sections from '@/app/admin/sections/sections';
import Create from '@/app/admin/sections/create';
import QueryAllSectionAction from '@/app/actions/sections/query-all-section-action';

export const metadata: Metadata = {
  title: 'section details - youdeyiwu',
  description: 'section page',
};

export default async function Page({
  searchParams,
}: {
  searchParams: {
    type?: 'add';
  };
}) {
  const type = searchParams.type;
  switch (type) {
    case 'add':
      return <Create />;
    default:
      return <Sections data={await QueryAllSectionAction()} />;
  }
}