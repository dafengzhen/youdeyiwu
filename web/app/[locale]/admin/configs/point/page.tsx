import { type Metadata } from 'next';
import PointConfig from '@/app/[locale]/admin/configs/point/point-config';
import QueryPointConfigAction from '@/app/[locale]/actions/configs/point/query-point-config-action';
import ErrorPage from '@/app/[locale]/common/error-page';

export const metadata: Metadata = {
  title: 'Point Config',
};

export default async function Page() {
  const response = await QueryPointConfigAction();
  if (response.isError) {
    return <ErrorPage message={response.message} />;
  }

  return <PointConfig config={response.data} />;
}
