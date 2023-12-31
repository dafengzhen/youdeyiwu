import { type Metadata } from 'next';
import SectionId from '@/app/sections/[id]/sectionid';
import QueryDetailsSectionAction from '@/app/actions/sections/query-details-section-action';
import {
  errorContent,
  errorTitle,
  getUserAlias,
  isNum,
  parseNum,
} from '@/app/common/server';
import { notFound } from 'next/navigation';
import LoginInfoUserAction from '@/app/actions/users/login-info-user-action';
import SelectAllPostAction from '@/app/actions/posts/select-all-post-action';
import QueryRandomPostAction from '@/app/actions/posts/query-random-post-action';
import { TQueryParams } from '@/app/interfaces';
import ClientErrorHandler from '@/app/common/client-error-handler';
import { ISectionDetails } from '@/app/interfaces/sections';

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

  let details: ISectionDetails;
  try {
    details = await QueryDetailsSectionAction({ id });
  } catch (e) {
    return errorTitle(e);
  }

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

  const queryParams: TQueryParams = {};
  const _searchParams = parseSearchParams(searchParams);
  if (typeof _searchParams.sectionGroupId !== 'number') {
    queryParams.sectionId = id;
  }
  if (typeof _searchParams.tagGroupId === 'number') {
    queryParams.tagGroupId = _searchParams.tagGroupId + '';
  }
  if (typeof _searchParams.tagId === 'number') {
    queryParams.tagId = _searchParams.tagId + '';
  }

  try {
    return (
      <SectionId
        details={await QueryDetailsSectionAction({ id })}
        currentUser={await LoginInfoUserAction()}
        data={await SelectAllPostAction(queryParams)}
        randomData={await QueryRandomPostAction()}
        queryParams={queryParams}
      />
    );
  } catch (e) {
    return <ClientErrorHandler message={errorContent(e)} />;
  }
}

function parseSearchParams(searchParams: ISearchParamsSectionIdPage) {
  const { sgid, sectionGroupId, tid, tagId, tgid, tagGroupId } = searchParams;

  const params = {
    sectionGroupId: sectionGroupId ?? sgid,
    tagId: tagId ?? tid,
    tagGroupId: tagGroupId ?? tgid,
  };

  return {
    sectionGroupId: parseNum(params.sectionGroupId),
    tagId: parseNum(params.tagId),
    tagGroupId: parseNum(params.tagGroupId),
  };
}
