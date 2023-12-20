import { type Metadata } from 'next';
import PostId from '@/app/posts/[id]/postid';
import QueryDetailsPostAction from '@/app/actions/posts/query-details-post-action';
import { getUserAlias, isNum } from '@/app/common/server';
import { notFound } from 'next/navigation';
import SelectAllSectionGroupAction from '@/app/actions/section-groups/select-all-section-group-action';
import SelectAllSectionAction from '@/app/actions/sections/select-all-section-action';
import QueryRandomPostAction from '@/app/actions/posts/query-random-post-action';
import LoginInfoUserAction from '@/app/actions/users/login-info-user-action';

export async function generateMetadata({
  params,
}: {
  params: { id: string };
}): Promise<Metadata> {
  const id = params.id;
  if (!isNum(id)) {
    notFound();
  }

  const details = await QueryDetailsPostAction({ id });
  const user = details.user;
  const userAlias = getUserAlias(user);

  return {
    title: details.name,
    authors: {
      url: user ? `/users/${user.id}` : '/users',
      name: getUserAlias(user),
    },
    creator: user ? `${userAlias}(ID. ${user.id})` : userAlias,
    description: details.overview ?? '',
    keywords: [
      ...(details.section ? [details.section.name] : []),
      ...details.tags.map((tag) => tag.name),
    ],
    category: details.section ? details.section.name : '',
    bookmarks: `/posts/${details.id}`,
  };
}

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
