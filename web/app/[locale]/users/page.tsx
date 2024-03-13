import { type Metadata } from 'next';
import Users from '@/app/[locale]/users/users';
import SelectAllUserAction from '@/app/[locale]/actions/users/select-all-user-action';
import ErrorPage from '@/app/[locale]/common/error-page';

export const metadata: Metadata = {
  title: 'Users',
};

export default async function Page() {
  const response = await SelectAllUserAction();
  if (response.isError) {
    return <ErrorPage message={response.message} />;
  }

  return <Users data={response.data} />;
}
