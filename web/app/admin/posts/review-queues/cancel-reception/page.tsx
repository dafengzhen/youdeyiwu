import { type Metadata } from 'next';
import { isNum } from '@/app/common/server';
import { notFound } from 'next/navigation';
import CancelReception from '@/app/admin/posts/review-queues/cancel-reception/cancel-reception';

export const metadata: Metadata = {
  title: 'Cancel Reception',
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

  return <CancelReception id={parseInt(id)} />;
}
