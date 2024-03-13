import { type Metadata } from 'next';
import SectionId from '@/app/[locale]/sections/[id]/sectionid';
import QueryDetailsSectionAction from '@/app/[locale]/actions/sections/query-details-section-action';
import {
  getUserAlias,
  incorrectMetadataTitle,
  isNum,
} from '@/app/[locale]/common/tool';
import { notFound } from 'next/navigation';
import LoginInfoUserAction from '@/app/[locale]/actions/users/login-info-user-action';
import SelectAllPostAction from '@/app/[locale]/actions/posts/select-all-post-action';
import QueryRandomPostAction from '@/app/[locale]/actions/posts/query-random-post-action';
import ErrorPage from '@/app/[locale]/common/error-page';
import queryString from 'query-string';

export interface ISearchParamsSectionIdPage {
  sgid?: string;
  sectionGroupId?: string;
  tid?: string;
  tagId?: string;
  tgid?: string;
  tagGroupId?: string;
}

export async function generateMetadata({
  params,
}: {
  params: { id: string };
}): Promise<Metadata> {
  const id = params.id;
  if (!isNum(id)) {
    notFound();
  }

  const response = await QueryDetailsSectionAction({ id });
  if (response.isError) {
    return incorrectMetadataTitle(response);
  }

  const details = response.data;
  const user = details.user;
  const userAlias = getUserAlias(user);

  return {
    title: details.name,
    authors: {
      url: user ? `/users/${user.id}` : '/users',
      name: userAlias,
    },
    creator: user ? `${userAlias}(ID. ${user.id})` : userAlias,
    description: details.overview ?? '',
    keywords: [...[details.name], ...details.tags.map((tag) => tag.name)],
    category: details.name,
    bookmarks: `/sections/${details.id}`,
  };
}

export default async function Page({
  params,
  searchParams,
}: {
  params: {
    id: string;
  };
  searchParams: ISearchParamsSectionIdPage;
}) {
  const id = params.id;
  if (!isNum(id)) {
    notFound();
  }

  const _params = parseSearchParams(searchParams);
  const responses = await Promise.all([
    QueryDetailsSectionAction({ id }),
    LoginInfoUserAction(),
    SelectAllPostAction(_params),
    QueryRandomPostAction(),
  ]);
  const sectionResponse = responses[0];
  const userResponse = responses[1];
  const postResponse = responses[2];
  const randomPostResponse = responses[3];

  if (sectionResponse.isError) {
    return <ErrorPage message={sectionResponse.message} />;
  }

  if (userResponse.isError) {
    return <ErrorPage message={userResponse.message} />;
  }

  if (postResponse.isError) {
    return <ErrorPage message={postResponse.message} />;
  }

  if (randomPostResponse.isError) {
    return <ErrorPage message={randomPostResponse.message} />;
  }

  return (
    <SectionId
      details={sectionResponse.data}
      currentUser={userResponse.data}
      data={postResponse.data}
      randomData={randomPostResponse.data}
      queryParams={_params}
    />
  );
}

const parseSearchParams = (searchParams: ISearchParamsSectionIdPage) => {
  const {
    sgid,
    sectionGroupId = sgid,
    tid,
    tagId = tid,
    tgid,
    tagGroupId = tgid,
  } = searchParams;
  const params = {
    sectionGroupId,
    tagId,
    tagGroupId,
  };

  const parse = queryString.parse(queryString.stringify(params), {
    parseNumbers: true,
  }) as Record<string, string | number>;
  return { ...parse };
};
