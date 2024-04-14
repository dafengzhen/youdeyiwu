import { type Metadata } from 'next';
import ErrorPage from '@/app/[locale]/common/error-page';
import QueryAllFileAction from '@/app/[locale]/actions/files/query-all-file-action';
import Files from '@/app/[locale]/admin/files/files';

export const metadata: Metadata = {
  title: 'Files',
};

export default async function Page() {
  const response = await QueryAllFileAction();
  if (response.isError) {
    return <ErrorPage message={response.message} />;
  }

  return <Files data={response.data} />;
}
