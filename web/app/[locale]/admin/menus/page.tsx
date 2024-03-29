import { type Metadata } from 'next';
import Create from '@/app/[locale]/admin/menus/create';
import Menus from '@/app/[locale]/admin/menus/menus';
import QueryAllMenuAction from '@/app/[locale]/actions/menus/query-all-menu-action';
import ErrorPage from '@/app/[locale]/common/error-page';

export const metadata: Metadata = {
  title: 'Menus',
};

export default async function Page({
  searchParams,
}: {
  searchParams: {
    type?: 'add';
  };
}) {
  const response = await QueryAllMenuAction();
  if (response.isError) {
    return <ErrorPage message={response.message} />;
  }

  const type = searchParams.type;
  switch (type) {
    case 'add':
      return <Create />;
    default:
      return <Menus data={response.data} />;
  }
}
