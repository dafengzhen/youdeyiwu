import { type Metadata } from 'next';
import Sections from '@/app/[locale]/sections/sections';
import SelectAllSectionGroupAction from '@/app/[locale]/actions/section-groups/select-all-section-group-action';
import SelectAllSectionAction from '@/app/[locale]/actions/sections/select-all-section-action';
import ErrorPage from '@/app/[locale]/common/error-page';

export const metadata: Metadata = {
  title: 'Contents',
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
    SelectAllSectionGroupAction(),
    SelectAllSectionAction({ sectionKey }),
  ]);
  const sectionGroupResponse = responses[0];
  const sectionResponse = responses[1];

  if (sectionGroupResponse.isError) {
    return <ErrorPage message={sectionGroupResponse.message} />;
  }

  if (sectionResponse.isError) {
    return <ErrorPage message={sectionResponse.message} />;
  }

  return (
    <Sections
      sectionGroups={sectionGroupResponse.data}
      sections={sectionResponse.data.filter(
        (item) => item.sectionGroups.length === 0,
      )}
    />
  );
}
