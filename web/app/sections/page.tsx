import { type Metadata } from 'next';
import Sections from '@/app/sections/sections';
import SelectAllSectionGroupAction from '@/app/actions/section-groups/select-all-section-group-action';
import SelectAllSectionAction from '@/app/actions/sections/select-all-section-action';

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
  return (
    <Sections
      sectionGroups={await SelectAllSectionGroupAction()}
      sections={(await SelectAllSectionAction({ sectionKey })).filter(
        (item) => item.sectionGroups.length === 0,
      )}
    />
  );
}
