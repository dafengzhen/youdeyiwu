import { Metadata } from 'next';
import Register from '@/app/register/register';

export const metadata: Metadata = {
  title: 'User Register',
};

export default async function Page() {
  return <Register />;
}
