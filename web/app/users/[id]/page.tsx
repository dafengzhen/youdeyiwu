import { type Metadata } from 'next';
import UserId from '@/app/users/[id]/userid';
import { isNum } from '@/app/common/server';
import { notFound } from 'next/navigation';
import QueryDetailsUserAction from '@/app/actions/users/query-details-user-action';
import LoginInfoUserAction from '@/app/actions/users/login-info-user-action';

export const metadata: Metadata = {
  title: 'user homepage - youdeyiwu',
  description: 'user homepage',
};

export default async function Page({
  params,
}: {
  params: {
    id: string;
  };
}) {
  const id = params.id;
  if (!isNum(id)) {
    notFound();
  }

  return (
    <UserId
      details={await QueryDetailsUserAction({ id })}
      currentUser={await LoginInfoUserAction()}
    />
  );
}
