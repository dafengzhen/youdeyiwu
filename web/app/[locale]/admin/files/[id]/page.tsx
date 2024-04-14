import { type Metadata } from 'next';
import { notFound } from 'next/navigation';
import { isNum } from '@/app/[locale]/common/tool';
import ErrorPage from '@/app/[locale]/common/error-page';
import Delete from '@/app/[locale]/admin/files/[id]/delete';
import QueryFileAction from '@/app/[locale]/actions/files/query-file-action';

export const metadata: Metadata = {
  title: 'Update File',
};

export default async function Page({
  params,
  searchParams,
}: {
  params: {
    id: string;
  };
  searchParams: {
    type?: 'del';
  };
}) {
  const id = params.id;
  if (!isNum(id)) {
    notFound();
  }

  const response = await QueryFileAction({ id });
  if (response.isError) {
    return <ErrorPage message={response.message} />;
  }

  const type = searchParams.type;
  switch (type) {
    case 'del':
      return <Delete file={response.data} />;
    default:
      notFound();
  }
}
