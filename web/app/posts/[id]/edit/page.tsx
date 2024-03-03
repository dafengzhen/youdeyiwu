import { type Metadata } from 'next';
import Save from '@/app/posts/save/save';
import { isNum } from '@/app/common/tool';
import { notFound } from 'next/navigation';
import SelectAllSectionAction from '@/app/actions/sections/select-all-section-action';
import QueryPostAction from '@/app/actions/posts/query-post-action';
import ErrorPage from '@/app/common/error-page';

export const metadata: Metadata = {
  title: 'Edit Article',
};

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

  return <Save post={postResponse.data} sections={sectionResponse.data} />;
}
