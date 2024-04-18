import { type Metadata } from 'next';
import ErrorPage from '@/app/[locale]/common/error-page';
import RootConfig from '@/app/[locale]/admin/configs/root/root-config';
import QueryDisableRegistrationRootConfigAction from '@/app/[locale]/actions/configs/root/query-disable-registration-root-config-action';

export const metadata: Metadata = {
  title: 'Root Config',
};

export default async function Page() {
  const response = await QueryDisableRegistrationRootConfigAction();
  if (response.isError) {
    return <ErrorPage message={response.message} />;
  }

  return (
    <RootConfig
      config={{
        disableRegistration: response.data,
      }}
    />
  );
}
