import { type Metadata } from 'next';
import Save from '@/app/[locale]/posts/save/save';
import { isNum } from '@/app/[locale]/common/tool';
import { notFound } from 'next/navigation';
import SelectAllSectionAction from '@/app/[locale]/actions/sections/select-all-section-action';
import QueryPostAction from '@/app/[locale]/actions/posts/query-post-action';
import ErrorPage from '@/app/[locale]/common/error-page';
import QueryCreateGuidePostConfigAction from '@/app/[locale]/actions/configs/post/query-create-guide-post-config-action';
import LoginInfoUserAction from '@/app/[locale]/actions/users/login-info-user-action';

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
    QueryCreateGuidePostConfigAction(),
    LoginInfoUserAction(),
  ]);
  const postResponse = responses[0];
  const sectionResponse = responses[1];
  const createGuideResponse = responses[2];
  const currentUserResponse = responses[3];

  if (postResponse.isError) {
    return <ErrorPage message={postResponse.message} />;
  }

  if (sectionResponse.isError) {
    return <ErrorPage message={sectionResponse.message} />;
  }

  if (createGuideResponse.isError) {
    return <ErrorPage message={createGuideResponse.message} />;
  }

  if (currentUserResponse.isError) {
    return <ErrorPage message={currentUserResponse.message} />;
  }

  return (
    <Save
      post={postResponse.data}
      sections={sectionResponse.data}
      createGuide={createGuideResponse.data}
      currentUser={currentUserResponse.data}
    />
  );
}
