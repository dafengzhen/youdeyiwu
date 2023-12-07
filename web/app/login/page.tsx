import { Metadata } from 'next';
import Login from '@/app/login/login';

export const metadata: Metadata = {
  title: 'login - youdeyiwu',
  description: 'login page',
};

export default async function Page() {
  return <Login />;
}
