import { type Metadata } from 'next';
import Receive from '@/app/admin/posts/review-queues/receive/receive';
import { isNum } from '@/app/common/server';
import { notFound } from 'next/navigation';

export const metadata: Metadata = {
  title: 'Receive',
};

export default async function Page({
  searchParams,
}: {
  searchParams: { postId: string };
}) {
  const postId = searchParams.postId;
  if (!isNum(postId)) {
    notFound();
  }

  return <Receive postId={parseInt(postId)} />;
}
