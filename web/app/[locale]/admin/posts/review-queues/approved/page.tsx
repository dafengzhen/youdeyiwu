import { type Metadata } from 'next';
import { isNum } from '@/app/[locale]/common/tool';
import { notFound } from 'next/navigation';
import Approved from '@/app/[locale]/admin/posts/review-queues/approved/approved';

export const metadata: Metadata = {
  title: 'Approved',
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

  return <Approved id={parseInt(id)} />;
}
