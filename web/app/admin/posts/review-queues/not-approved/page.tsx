import { type Metadata } from 'next';
import { isNum } from '@/app/common/tool';
import { notFound } from 'next/navigation';
import NotApproved from '@/app/admin/posts/review-queues/not-approved/not-approved';

export const metadata: Metadata = {
  title: 'Not Approved',
};

export default async function Page({
  searchParams,
}: {
  searchParams: { id: string };
}) {
  const id = searchParams.id;
  if (!isNum(id)) {
    notFound();
  }

  return <NotApproved id={parseInt(id)} />;
}
