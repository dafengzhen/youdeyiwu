import { type Metadata } from 'next';
import Delete from '@/app/[locale]/admin/posts/[id]/delete';
import { notFound } from 'next/navigation';
import { isNum } from '@/app/[locale]/common/tool';
import QueryPostAction from '@/app/[locale]/actions/posts/query-post-action';
import UpdateStates from '@/app/[locale]/admin/posts/[id]/update-states';
import UpdateTags from '@/app/[locale]/admin/posts/[id]/update-tags';
import UpdateSection from '@/app/[locale]/admin/posts/[id]/update-section';
import SelectAllSectionAction from '@/app/[locale]/actions/sections/select-all-section-action';
import ErrorPage from '@/app/[locale]/common/error-page';

export const metadata: Metadata = {
  title: 'Update Post',
};

export default async function Page({
  params,
  searchParams,
}: {
  params: {
    id: string;
  };
  searchParams: {
    type?: 'del' | 'states' | 'tags' | 'section';
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
    QueryPostAction({ id }),
    SelectAllSectionAction({ sectionKey }),
  ]);
  const postResponse = responses[0];
  const sectionResponse = responses[1];

  if (postResponse.isError) {
    return <ErrorPage message={postResponse.message} />;
  }

  if (sectionResponse.isError) {
    return <ErrorPage message={sectionResponse.message} />;
  }

  const type = searchParams.type;
  switch (type) {
    case 'del':
      return <Delete post={postResponse.data} />;
    case 'states':
      return <UpdateStates post={postResponse.data} />;
    case 'tags':
      return <UpdateTags post={postResponse.data} />;
    case 'section':
      return (
        <UpdateSection
          post={postResponse.data}
          sections={sectionResponse.data}
        />
      );
    default:
      notFound();
  }
}
