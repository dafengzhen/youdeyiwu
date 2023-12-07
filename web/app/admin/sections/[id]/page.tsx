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

export const metadata: Metadata = {
  title: 'update sections - youdeyiwu',
  description: 'update sections page',
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

  const section = await QuerySectionAction({ id });
  const type = searchParams.type;
  switch (type) {
    case 'del':
      return <Delete section={section} />;
    case 'states':
      return <UpdateStates section={section} />;
    case 'admins':
      return <UpdateAdmins section={section} />;
    case 'tags':
      return <UpdateTags section={section} />;
    case 'tagGroups':
      return <UpdateTagGroups section={section} />;
    default:
      return <Update section={section} />;
  }
}
