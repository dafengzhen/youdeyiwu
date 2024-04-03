import { type Metadata } from 'next';
import Save from '@/app/[locale]/posts/save/save';
import SelectAllSectionAction from '@/app/[locale]/actions/sections/select-all-section-action';
import ErrorPage from '@/app/[locale]/common/error-page';
import QueryCreateGuidePostConfigAction from '@/app/[locale]/actions/configs/post/query-create-guide-post-config-action';
import LoginInfoUserAction from '@/app/[locale]/actions/users/login-info-user-action';

export const metadata: Metadata = {
  title: 'Create Article',
};

export default async function Page({
  searchParams,
}: {
  searchParams: {
    sKey?: string;
    sectionKey?: string;
  };
}) {
  const sectionKey = searchParams.sectionKey ?? searchParams.sKey;

  const responses = await Promise.all([
    SelectAllSectionAction({ sectionKey }),
    QueryCreateGuidePostConfigAction(),
    LoginInfoUserAction(),
  ]);

  const sectionsResponse = responses[0];
  const createGuideResponse = responses[1];
  const currentUserResponse = responses[2];

  if (sectionsResponse.isError) {
    return <ErrorPage message={sectionsResponse.message} />;
  }

  if (createGuideResponse.isError) {
    return <ErrorPage message={createGuideResponse.message} />;
  }

  if (currentUserResponse.isError) {
    return <ErrorPage message={currentUserResponse.message} />;
  }

  return (
    <Save
      sections={sectionsResponse.data}
      createGuide={createGuideResponse.data}
      currentUser={currentUserResponse.data}
    />
  );
}
