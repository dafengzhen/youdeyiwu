import Home from '@/app/home/home';
import { type Metadata } from 'next';
import QueryRandomPostAction from '@/app/actions/posts/query-random-post-action';
import SelectAllSectionGroupAction from '@/app/actions/section-groups/select-all-section-group-action';
import SelectAllSectionAction from '@/app/actions/sections/select-all-section-action';
import SelectAllTagAction from '@/app/actions/tags/select-all-tag-action';
import SelectAllPostAction from '@/app/actions/posts/select-all-post-action';
import { parseNum } from '@/app/common/server';
import type { TQueryParams } from '@/app/interfaces';
import LoginInfoUserAction from '@/app/actions/users/login-info-user-action';

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
  title: 'home - youdeyiwu',
  description: 'youdeyiwu is an open-source lightweight forum',
};

export default async function Page({
  searchParams,
}: {
  searchParams: ISearchParamsHomePage;
}) {
  const params = parseSearchParams(searchParams);
  const queryParams: TQueryParams = {};
  const sectionGroups = await SelectAllSectionGroupAction();
  let sections = await SelectAllSectionAction({
    sectionKey: params.sectionKey,
  });
  let tags = await SelectAllTagAction();

  const sectionGroupId = params.sectionGroupId;
  if (typeof sectionGroupId === 'number') {
    const find = sectionGroups.find((item) => item.id === sectionGroupId);
    if (find) {
      queryParams.sectionGroupId = sectionGroupId + '';
      sections = find.sections;
    }
  }

  const sectionId = params.sectionId;
  if (typeof sectionId === 'number') {
    const find = sections.find((item) => item.id === sectionId);
    if (find) {
      queryParams.sectionId = sectionId + '';
      tags = find.tags;
    }
  }

  const tagId = params.tagId;
  if (typeof tagId === 'number') {
    queryParams.tagId = tagId + '';
  }

  return (
    <Home
      sectionGroups={sectionGroups}
      sections={sections}
      tags={tags}
      data={await SelectAllPostAction(queryParams)}
      randomData={await QueryRandomPostAction()}
      queryParams={queryParams}
      currentUser={await LoginInfoUserAction()}
    />
  );
}

function parseSearchParams(searchParams: ISearchParamsHomePage) {
  const { sgid, sectionGroupId, sid, sectionId, tid, tagId, sKey, sectionKey } =
    searchParams;

  const params = {
    sectionGroupId: sectionGroupId ?? sgid,
    sectionId: sectionId ?? sid,
    tagId: tagId ?? tid,
    sectionKey: sKey ?? sectionKey,
  };

  return {
    sectionGroupId: parseNum(params.sectionGroupId),
    sectionId: parseNum(params.sectionId),
    tagId: parseNum(params.tagId),
    sectionKey: params.sectionKey,
  };
}
