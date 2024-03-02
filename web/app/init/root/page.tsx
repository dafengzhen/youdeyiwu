import { type Metadata } from 'next';
import InitRoot from '@/app/init/root/root';
import LoginInfoUserAction from '@/app/actions/users/login-info-user-action';
import ErrorPage from '@/app/common/error-page';

export const metadata: Metadata = {
  title: 'Init Root',
  description: 'Initialize Forum Administrator',
};

export default async function Page() {
  const response = await LoginInfoUserAction();
  if (response.isError) {
    return <ErrorPage message={response.message} />;
  }

  return <InitRoot currentUser={response.data} />;
}
