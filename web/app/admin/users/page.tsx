import { type Metadata } from 'next';
import Users from '@/app/admin/users/users';
import QueryAllUserAction from '@/app/actions/users/query-all-user-action';
import ErrorPage from '@/app/common/error-page';

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
