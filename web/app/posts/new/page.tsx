import { type Metadata } from 'next';
import Save from '@/app/posts/save/save';
import SelectAllSectionAction from '@/app/actions/sections/select-all-section-action';

export const metadata: Metadata = {
  title: 'create article - youdeyiwu',
  description: 'create article page',
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
  return <Save sections={await SelectAllSectionAction({ sectionKey })} />;
}
