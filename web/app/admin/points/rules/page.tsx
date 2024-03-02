import { type Metadata } from 'next';
import PointRules from '@/app/admin/points/rules/point-rules';
import QueryRulesPointsAction from '@/app/actions/points/rules/query-rules-points-action';
import ErrorPage from '@/app/common/error-page';

export const metadata: Metadata = {
  title: 'Point Auto Rule',
};

export default async function Page() {
  const response = await QueryRulesPointsAction();
  if (response.isError) {
    return <ErrorPage message={response.message} />;
  }

  return <PointRules data={response.data} />;
}
