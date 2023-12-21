import { type Metadata } from 'next';
import Save from '@/app/posts/save/save';
import { isNum } from '@/app/common/server';
import { notFound } from 'next/navigation';
import SelectAllSectionAction from '@/app/actions/sections/select-all-section-action';
import QueryPostAction from '@/app/actions/posts/query-post-action';

export const metadata: Metadata = {
  title: 'edit article - youdeyiwu',
  description: 'edit article page',
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
    <Save
      post={await QueryPostAction({ id })}
      sections={await SelectAllSectionAction()}
    />
  );
}
