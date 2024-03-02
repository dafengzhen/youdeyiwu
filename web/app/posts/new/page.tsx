import { type Metadata } from 'next';
import Save from '@/app/posts/save/save';
import SelectAllSectionAction from '@/app/actions/sections/select-all-section-action';
import ErrorPage from '@/app/common/error-page';

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
  const response = await SelectAllSectionAction({ sectionKey });
  if (response.isError) {
    return <ErrorPage message={response.message} />;
  }

  return <Save sections={response.data} />;
}
