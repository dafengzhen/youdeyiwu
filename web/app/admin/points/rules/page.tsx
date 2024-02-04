import { type Metadata } from 'next';
import PointRules from '@/app/admin/points/rules/point-rules';
import QueryRulesPointsAction from '@/app/actions/points/rules/query-rules-points-action';

export const metadata: Metadata = {
  title: 'Point Auto Rule',
};

export default async function Page() {
  return <PointRules data={await QueryRulesPointsAction()} />;
}
