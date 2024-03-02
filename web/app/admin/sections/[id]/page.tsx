import { type Metadata } from 'next';
import Update from '@/app/admin/sections/[id]/update';
import Delete from '@/app/admin/sections/[id]/delete';
import { notFound } from 'next/navigation';
import { isNum } from '@/app/common/server';
import UpdateStates from '@/app/admin/sections/[id]/update-states';
import UpdateAdmins from '@/app/admin/sections/[id]/update-admins';
import QuerySectionAction from '@/app/actions/sections/query-section-action';
import UpdateTags from '@/app/admin/sections/[id]/update-tags';
import UpdateTagGroups from '@/app/admin/sections/[id]/update-tag-groups';
import ErrorPage from '@/app/common/error-page';

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
    type?: 'del' | 'states' | 'admins' | 'tags' | 'tagGroups';
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
    default:
      return <Update section={response.data} />;
  }
}
