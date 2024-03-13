import { type Metadata } from 'next';
import Create from '@/app/[locale]/admin/submenus/create';
import Submenus from '@/app/[locale]/admin/submenus/submenus';
import QueryAllSubmenuAction from '@/app/[locale]/actions/submenus/query-all-submenu-action';
import ErrorPage from '@/app/[locale]/common/error-page';

export const metadata: Metadata = {
  title: 'Submenus',
};

export default async function Page({
  searchParams,
}: {
  searchParams: {
    type?: 'add';
  };
}) {
  const response = await QueryAllSubmenuAction();
  if (response.isError) {
    return <ErrorPage message={response.message} />;
  }

  const type = searchParams.type;
  switch (type) {
    case 'add':
      return <Create />;
    default:
      return <Submenus data={response.data} />;
  }
}
