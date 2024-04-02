import { type Metadata } from 'next';
import ErrorPage from '@/app/[locale]/common/error-page';
import QueryCreateGuidePostConfigAction from '@/app/[locale]/actions/configs/post/query-create-guide-post-config-action';
import CreateGuidePostConfig from '@/app/[locale]/admin/configs/post/create-guide/create-guide';

export const metadata: Metadata = {
  title: 'Post Config',
};

export default async function Page() {
  const response = await QueryCreateGuidePostConfigAction();
  if (response.isError) {
    return <ErrorPage message={response.message} />;
  }

  return (
    <CreateGuidePostConfig
      config={{
        createGuide: response.data,
      }}
    />
  );
}
