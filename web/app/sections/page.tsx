import { type Metadata } from 'next';
import Sections from '@/app/sections/sections';
import SelectAllSectionGroupAction from '@/app/actions/section-groups/select-all-section-group-action';
import SelectAllSectionAction from '@/app/actions/sections/select-all-section-action';

export const metadata: Metadata = {
  title: 'contents - youdeyiwu',
  description: 'view all content',
};

export default async function Page() {
  return (
    <Sections
      sectionGroups={await SelectAllSectionGroupAction()}
      sections={(await SelectAllSectionAction()).filter(
        (item) => item.sectionGroups.length === 0,
      )}
    />
  );
}
