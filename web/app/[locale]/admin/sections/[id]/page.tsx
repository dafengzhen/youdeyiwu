import { type Metadata } from 'next';
import Update from '@/app/[locale]/admin/sections/[id]/update';
import Delete from '@/app/[locale]/admin/sections/[id]/delete';
import { notFound } from 'next/navigation';
import { isNum } from '@/app/[locale]/common/tool';
import UpdateStates from '@/app/[locale]/admin/sections/[id]/update-states';
import UpdateAdmins from '@/app/[locale]/admin/sections/[id]/update-admins';
import QuerySectionAction from '@/app/[locale]/actions/sections/query-section-action';
import UpdateTags from '@/app/[locale]/admin/sections/[id]/update-tags';
import UpdateTagGroups from '@/app/[locale]/admin/sections/[id]/update-tag-groups';
import ErrorPage from '@/app/[locale]/common/error-page';
import UpdateCreatePostGuide from '@/app/[locale]/admin/sections/[id]/update-create-post-guide';

export const metadata: Metadata = {
  title: 'Update Section',
};

export default async function Page({
  params,
  searchParams,
}: {
  params: {
    id: string;
  };
  searchParams: {
    type?:
      | 'del'
      | 'states'
      | 'admins'
      | 'tags'
      | 'tagGroups'
      | 'createPostGuide';
  };
}) {
  const id = params.id;
  if (!isNum(id)) {
    notFound();
  }

  const response = await QuerySectionAction({ id });
  if (response.isError) {
    return <ErrorPage message={response.message} />;
  }

  const type = searchParams.type;
  switch (type) {
    case 'del':
      return <Delete section={response.data} />;
    case 'states':
      return <UpdateStates section={response.data} />;
    case 'admins':
      return <UpdateAdmins section={response.data} />;
    case 'tags':
      return <UpdateTags section={response.data} />;
    case 'tagGroups':
      return <UpdateTagGroups section={response.data} />;
    case 'createPostGuide':
      return <UpdateCreatePostGuide section={response.data} />;
    default:
      return <Update section={response.data} />;
  }
}
