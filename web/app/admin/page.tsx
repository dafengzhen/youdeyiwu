import { type Metadata } from 'next';
import Admin from '@/app/admin/admin';
import CountByDateUserAction from '@/app/actions/users/count-by-date-user-action';

export const metadata: Metadata = {
  title: 'Admin',
};

export default async function Page() {
  return <Admin usersCountByDate={await CountByDateUserAction()} />;
}
