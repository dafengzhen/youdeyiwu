import { type Metadata } from 'next';
import Users from '@/app/[locale]/admin/users/users';
import QueryAllUserAction from '@/app/[locale]/actions/users/query-all-user-action';
import ErrorPage from '@/app/[locale]/common/error-page';

export const metadata: Metadata = {
  title: 'Users',
};

export default async function Page() {
  const response = await QueryAllUserAction();
  if (response.isError) {
    return <ErrorPage message={response.message} />;
  }

  return <Users data={response.data} />;
}
