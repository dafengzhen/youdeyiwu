import { type Metadata } from 'next';
import InitRoot from '@/app/init/root/root';
import LoginInfoUserAction from '@/app/actions/users/login-info-user-action';

export const metadata: Metadata = {
  title: 'Init Root',
  description: 'Initialize Forum Administrator',
};

export default async function Page() {
  return <InitRoot currentUser={await LoginInfoUserAction()} />;
}
