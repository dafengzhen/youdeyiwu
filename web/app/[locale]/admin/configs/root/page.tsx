import { type Metadata } from 'next';
import ErrorPage from '@/app/[locale]/common/error-page';
import RootConfig from '@/app/[locale]/admin/configs/root/root-config';
import QueryDisableRegistrationRootConfigAction from '@/app/[locale]/actions/configs/root/query-disable-registration-root-config-action';
import QueryRootConfigAction from '@/app/[locale]/actions/configs/root/query-root-config-action';

export const metadata: Metadata = {
  title: 'Root Config',
};

export default async function Page() {
  const responses = await Promise.all([
    QueryDisableRegistrationRootConfigAction(),
    QueryRootConfigAction({
      disableAnonymous: true,
    }),
  ]);

  const disableRegistrationResponse = responses[0];
  const disableAnonymousResponse = responses[1];

  if (disableRegistrationResponse.isError) {
    return <ErrorPage message={disableRegistrationResponse.message} />;
  }

  if (disableAnonymousResponse.isError) {
    return <ErrorPage message={disableAnonymousResponse.message} />;
  }

  const disableAnonymous = disableAnonymousResponse.data;
  return (
    <RootConfig
      config={{
        disableRegistration: disableRegistrationResponse.data,
        disableAnonymousPosts: disableAnonymous.disableAnonymousPosts,
        disableAnonymousComments: disableAnonymous.disableAnonymousComments,
        disableAnonymousReplies: disableAnonymous.disableAnonymousReplies,
      }}
    />
  );
}
