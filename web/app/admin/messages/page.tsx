import { type Metadata } from 'next';
import Create from '@/app/admin/messages/create';
import QueryAllGlobalMessageAction from '@/app/actions/messages/query-all-global-message-action';
import Messages from '@/app/admin/messages/messages';

export const metadata: Metadata = {
  title: 'Messages',
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
      return <Messages data={await QueryAllGlobalMessageAction()} />;
  }
}
