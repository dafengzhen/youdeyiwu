import { type Metadata } from 'next';
import Create from '@/app/admin/actions/create';
import Actions from '@/app/admin/actions/actions';
import QueryAllActionAction from '@/app/actions/actions/query-all-action-action';
import ErrorPage from '@/app/common/error-page';

export const metadata: Metadata = {
  title: 'Actions',
};

export default async function Page({
  searchParams,
}: {
  searchParams: {
    type?: 'add';
  };
}) {
  const response = await QueryAllActionAction();
  if (response.isError) {
    return <ErrorPage message={response.message} />;
  }

  const type = searchParams.type;
  switch (type) {
    case 'add':
      return <Create />;
    default:
      return <Actions data={response.data} />;
  }
}
