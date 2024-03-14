import { type Metadata } from 'next';
import PostId from '@/app/[locale]/posts/[id]/postid';
import QueryDetailsPostAction from '@/app/[locale]/actions/posts/query-details-post-action';
import {
  getUserAlias,
  incorrectMetadataTitle,
  isNum,
} from '@/app/[locale]/common/tool';
import { notFound } from 'next/navigation';
import SelectAllSectionGroupAction from '@/app/[locale]/actions/section-groups/select-all-section-group-action';
import SelectAllSectionAction from '@/app/[locale]/actions/sections/select-all-section-action';
import QueryRandomPostAction from '@/app/[locale]/actions/posts/query-random-post-action';
import LoginInfoUserAction from '@/app/[locale]/actions/users/login-info-user-action';
import ErrorPage from '@/app/[locale]/common/error-page';

export async function generateMetadata({
  params,
}: {
  params: { id: string };
}): Promise<Metadata> {
  const id = params.id;
  if (!isNum(id)) {
    notFound();
  }

  const response = await QueryDetailsPostAction({ id });
  if (response.isError) {
    return incorrectMetadataTitle(response);
  }

  const details = response.data;
  const user = details.user;
  const userAlias = getUserAlias(user);

  const url = process.env.URL + `/posts/${details.id}`;
  const title = details.name;
  const description = details.overview ?? '';
  const publishedTime = details.createdOn;

  return {
    title,
    description,
    authors: {
      url: user ? `/users/${user.id}` : '/users',
      name: userAlias,
    },
    creator: user ? `${userAlias}(ID. ${user.id})` : userAlias,
    keywords: [
      ...(details.section ? [details.section.name] : []),
      ...details.tags.map((tag) => tag.name),
    ],
    category: details.section ? details.section.name : '',
    bookmarks: url,
    openGraph: {
      url,
      title,
      description,
      type: 'article',
      images: details.cover
        ? {
            url: details.cover,
            alt: 'cover',
          }
        : undefined,
      publishedTime,
      authors: userAlias,
    },
  };
}

export default async function Page({
  params,
  searchParams,
}: {
  params: {
    id: string;
  };
  searchParams: {
    sKey?: string;
    sectionKey?: string;
  };
}) {
  const id = params.id;
  if (!isNum(id)) {
    notFound();
  }

  const sectionKey = searchParams.sectionKey ?? searchParams.sKey;
  const responses = await Promise.all([
    SelectAllSectionGroupAction(),
    SelectAllSectionAction({ sectionKey }),
    QueryRandomPostAction(),
    QueryDetailsPostAction({ id }),
    LoginInfoUserAction(),
  ]);
  const sectionGroupResponse = responses[0];
  const sectionResponse = responses[1];
  const randomPostResponse = responses[2];
  const postResponse = responses[3];
  const userResponse = responses[4];

  if (sectionGroupResponse.isError) {
    return <ErrorPage message={sectionGroupResponse.message} />;
  }

  if (sectionResponse.isError) {
    return <ErrorPage message={sectionResponse.message} />;
  }

  if (randomPostResponse.isError) {
    return <ErrorPage message={randomPostResponse.message} />;
  }

  if (postResponse.isError) {
    return <ErrorPage message={postResponse.message} />;
  }

  if (userResponse.isError) {
    return <ErrorPage message={userResponse.message} />;
  }

  return (
    <PostId
      sectionGroups={sectionGroupResponse.data}
      sections={sectionResponse.data}
      randomData={randomPostResponse.data}
      details={postResponse.data}
      currentUser={userResponse.data}
    />
  );
}
