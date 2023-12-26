import { type Metadata } from 'next';
import Configs from '@/app/admin/configs/configs';

export const metadata: Metadata = {
  title: 'Configs',
};

export default async function Page() {
  return <Configs />;
}
