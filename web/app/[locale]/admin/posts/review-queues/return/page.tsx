import { type Metadata } from 'next';
import { isNum } from '@/app/[locale]/common/tool';
import { notFound } from 'next/navigation';
import Return from '@/app/[locale]/admin/posts/review-queues/return/return';

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

  return <Return id={parseInt(id)} />;
}
