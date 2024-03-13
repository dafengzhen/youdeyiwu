import { type Metadata } from 'next';
import InitRoot from '@/app/[locale]/init/root/root';
import LoginInfoUserAction from '@/app/[locale]/actions/users/login-info-user-action';
import ErrorPage from '@/app/[locale]/common/error-page';

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
