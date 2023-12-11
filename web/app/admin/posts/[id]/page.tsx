import { type Metadata } from 'next';
import Delete from '@/app/admin/posts/[id]/delete';
import { notFound } from 'next/navigation';
import { isNum } from '@/app/common/server';
import QueryPostAction from '@/app/actions/posts/query-post-action';
import UpdateStates from '@/app/admin/posts/[id]/update-states';
import UpdateTags from '@/app/admin/posts/[id]/update-tags';
import UpdateSection from '@/app/admin/posts/[id]/update-section';
import SelectAllSectionAction from '@/app/actions/sections/select-all-section-action';

export const metadata: Metadata = {
  title: 'post details - youdeyiwu',
  description: 'post details page',
};

export default async function Page({
  params,
  searchParams,
}: {
  params: {
    id: string;
  };
  searchParams: {
    type?: 'del' | 'states' | 'tags' | 'section';
  };
}) {
  const id = params.id;
  if (!isNum(id)) {
    notFound();
  }

  const post = await QueryPostAction({ id });
  const type = searchParams.type;
  switch (type) {
    case 'del':
      return <Delete post={post} />;
    case 'states':
      return <UpdateStates post={post} />;
    case 'tags':
      return <UpdateTags post={post} />;
    case 'section':
      return (
        <UpdateSection post={post} sections={await SelectAllSectionAction()} />
      );
    default:
      notFound();
  }
}