import { Metadata } from 'next';
import Login from '@/app/[locale]/login/login';

export const metadata: Metadata = {
  title: 'User Login',
};

export default async function Page() {
  return <Login />;
}
