import { type Metadata } from 'next';
import Create from '@/app/admin/actions/create';
import Actions from '@/app/admin/actions/actions';
import QueryAllActionAction from '@/app/actions/actions/query-all-action-action';

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
  const type = searchParams.type;
  switch (type) {
    case 'add':
      return <Create />;
    default:
      return <Actions data={await QueryAllActionAction()} />;
  }
}
