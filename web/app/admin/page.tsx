import { type Metadata } from 'next';
import Admin from '@/app/admin/admin';
import CountByDateUserAction from '@/app/actions/users/count-by-date-user-action';
import ErrorPage from '@/app/common/error-page';

export const metadata: Metadata = {
  title: 'Admin',
};

export default async function Page() {
  const response = await CountByDateUserAction();
  if (response.isError) {
    return <ErrorPage message={response.message} />;
  }

  return <Admin usersCountByDate={response.data} />;
}
