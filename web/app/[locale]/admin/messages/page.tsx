import { type Metadata } from 'next';
import Create from '@/app/[locale]/admin/messages/create';
import QueryAllGlobalMessageAction from '@/app/[locale]/actions/messages/query-all-global-message-action';
import Messages from '@/app/[locale]/admin/messages/messages';
import ErrorPage from '@/app/[locale]/common/error-page';

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
  const response = await QueryAllGlobalMessageAction();
  if (response.isError) {
    return <ErrorPage message={response.message} />;
  }

  const type = searchParams.type;
  switch (type) {
    case 'add':
      return <Create />;
    default:
      return <Messages data={response.data} />;
  }
}
