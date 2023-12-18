import { type Metadata } from 'next';
import { notFound } from 'next/navigation';
import { isNum } from '@/app/common/server';
import QueryGlobalMessageAction from '@/app/actions/messages/query-global-message-action';
import Details from '@/app/admin/messages/global-messages/[id]/details';

export const metadata: Metadata = {
  title: 'global message - youdeyiwu',
  description: 'global message page',
};

export default async function Page({
  params,
  searchParams,
}: {
  params: {
    id: string;
  };
  searchParams: {
    type?: 'details';
  };
}) {
  const id = params.id;
  if (!isNum(id)) {
    notFound();
  }

  const type = searchParams.type;
  if (type !== 'details') {
    notFound();
  }

  const message = await QueryGlobalMessageAction({ id });
  return <Details message={message} />;
}
