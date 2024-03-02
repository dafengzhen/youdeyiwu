import Home from '@/app/home/home';
import { type Metadata } from 'next';
import QueryRandomPostAction from '@/app/actions/posts/query-random-post-action';
import SelectAllSectionGroupAction from '@/app/actions/section-groups/select-all-section-group-action';
import SelectAllSectionAction from '@/app/actions/sections/select-all-section-action';
import SelectAllTagAction from '@/app/actions/tags/select-all-tag-action';
import SelectAllPostAction from '@/app/actions/posts/select-all-post-action';
import LoginInfoUserAction from '@/app/actions/users/login-info-user-action';
import ErrorPage from '@/app/common/error-page';
import type { ISectionGroup } from '@/app/interfaces/section-groups';
import type { ISection } from '@/app/interfaces/sections';
import { createSuccessResponse } from '@/app/common/response';
import queryString from 'query-string';

export interface ISearchParamsHomePage {
  sgid?: string;
  sectionGroupId?: string;
  sid?: string;
  sectionId?: string;
  tid?: string;
  tagId?: string;
  sKey?: string;
  sectionKey?: string;
}

export const metadata: Metadata = {
  title: `Home | ${process.env.NAME!}`,
  description: process.env.DESCRIPTION!,
};

export default async function Page({
  searchParams,
}: {
  searchParams: ISearchParamsHomePage;
}) {
  const params = parseSearchParams(searchParams);

  const sectionGroupResponse = await fetchSectionGroup();
  if (sectionGroupResponse.isError) {
    return <ErrorPage message={sectionGroupResponse.message} />;
  }

  const sectionResponse = await fetchSections(
    params,
    sectionGroupResponse.data,
  );
  if (sectionResponse.isError) {
    return <ErrorPage message={sectionResponse.message} />;
  }

  const tagResponse = await fetchTags(params, sectionResponse.data);
  if (tagResponse.isError) {
    return <ErrorPage message={tagResponse.message} />;
  }

  const responses = await Promise.all([
    SelectAllPostAction(params),
    QueryRandomPostAction(),
    LoginInfoUserAction(),
  ]);

  const postResponse = responses[0];
  if (postResponse.isError) {
    return <ErrorPage message={postResponse.message} />;
  }

  const randomPostResponse = responses[1];
  if (randomPostResponse.isError) {
    return <ErrorPage message={randomPostResponse.message} />;
  }

  const loginInfoUserResponse = responses[2];
  if (loginInfoUserResponse.isError) {
    return <ErrorPage message={loginInfoUserResponse.message} />;
  }

  return (
    <Home
      sectionGroups={sectionGroupResponse.data}
      sections={sectionResponse.data}
      tags={tagResponse.data}
      data={postResponse.data}
      randomData={randomPostResponse.data}
      queryParams={params}
      currentUser={loginInfoUserResponse.data}
    />
  );
}

const fetchSectionGroup = async () => {
  return SelectAllSectionGroupAction();
};

const fetchSections = async (params: any, sectionGroups: ISectionGroup[]) => {
  if (typeof params.sectionGroupId === 'number') {
    const find = sectionGroups.find(
      (item) => item.id === params.sectionGroupId,
    );
    if (find) {
      return createSuccessResponse(find.sections);
    }
  }

  return SelectAllSectionAction({
    sectionKey: params.sectionKey as string,
  });
};

const fetchTags = async (params: any, sections: ISection[]) => {
  if (typeof params.sectionId === 'number') {
    const find = sections.find((item) => item.id === params.sectionId);
    if (find) {
      return createSuccessResponse(find.tags);
    }
  }

  return SelectAllTagAction();
};

const parseSearchParams = (searchParams: ISearchParamsHomePage) => {
  const {
    sgid,
    sectionGroupId = sgid,
    sid,
    sectionId = sid,
    tid,
    tagId = tid,
    sKey,
    sectionKey = sKey,
  } = searchParams;
  const params = {
    sectionGroupId,
    sectionId,
    tagId,
    sectionKey,
  };

  const parse = queryString.parse(queryString.stringify(params), {
    parseNumbers: true,
  }) as Record<string, string | number>;
  return { ...parse };
};
