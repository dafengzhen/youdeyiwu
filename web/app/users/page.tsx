import { type Metadata } from 'next';
import Users from '@/app/users/users';
import SelectAllUserAction from '@/app/actions/users/select-all-user-action';

export const metadata: Metadata = {
  title: 'Users',
};

export default async function Page() {
  return <Users data={await SelectAllUserAction()} />;
}
