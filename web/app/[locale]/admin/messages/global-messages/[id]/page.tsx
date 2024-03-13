import { type Metadata } from 'next';
import { notFound } from 'next/navigation';
import { isNum } from '@/app/[locale]/common/tool';
import QueryGlobalMessageAction from '@/app/[locale]/actions/messages/query-global-message-action';
import Details from '@/app/[locale]/admin/messages/global-messages/[id]/details';
import ErrorPage from '@/app/[locale]/common/error-page';

export const metadata: Metadata = {
  title: 'Global Message',
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

  const response = await QueryGlobalMessageAction({ id });
  if (response.isError) {
    return <ErrorPage message={response.message} />;
  }

  return <Details message={response.data} />;
}
