import { type Metadata } from 'next';
import PointPermissionRules from '@/app/[locale]/admin/points/permission-rules/point-permission-rules';
import QueryPermissionRulesPointsAction from '@/app/[locale]/actions/points/permission-rules/query-permission-rules-points-action';
import ErrorPage from '@/app/[locale]/common/error-page';

export const metadata: Metadata = {
  title: 'Point Rule',
};

export default async function Page() {
  const response = await QueryPermissionRulesPointsAction();
  if (response.isError) {
    return <ErrorPage message={response.message} />;
  }

  return <PointPermissionRules data={response.data} />;
}
