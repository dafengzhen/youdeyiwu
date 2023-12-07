import { type Metadata } from 'next';
import PostId from '@/app/posts/[id]/postid';
import QueryDetailsPostAction from '@/app/actions/posts/query-details-post-action';
import { isNum } from '@/app/common/server';
import { notFound } from 'next/navigation';
import SelectAllSectionGroupAction from '@/app/actions/section-groups/select-all-section-group-action';
import SelectAllSectionAction from '@/app/actions/sections/select-all-section-action';
import QueryRandomPostAction from '@/app/actions/posts/query-random-post-action';
import LoginInfoUserAction from '@/app/actions/users/login-info-user-action';

export const metadata: Metadata = {
  title: 'article details page - youdeyiwu',
  description: 'article details page',
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
    <PostId
      sectionGroups={await SelectAllSectionGroupAction()}
      sections={await SelectAllSectionAction()}
      randomData={await QueryRandomPostAction()}
      details={await QueryDetailsPostAction({ id })}
      currentUser={await LoginInfoUserAction()}
    />
  );
}
