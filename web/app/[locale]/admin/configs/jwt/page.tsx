import { type Metadata } from 'next';
import JwtConfig from '@/app/[locale]/admin/configs/jwt/jwt-config';
import QueryJwtConfigAction from '@/app/[locale]/actions/configs/jwt/query-jwt-config-action';
import ErrorPage from '@/app/[locale]/common/error-page';

export const metadata: Metadata = {
  title: 'Jwt Config',
};

export default async function Page() {
  const response = await QueryJwtConfigAction();
  if (response.isError) {
    return <ErrorPage message={response.message} />;
  }

  return <JwtConfig config={response.data} />;
}
