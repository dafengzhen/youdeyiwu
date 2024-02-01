import { type Metadata } from 'next';
import PointAutoRules from '@/app/admin/points/auto-rules/point-auto-rules';
import QueryAutoRulesPointsAction from '@/app/actions/points/auto-rules/query-auto-rules-points-action';

export const metadata: Metadata = {
  title: 'Point Auto Rule',
};

export default async function Page() {
  return <PointAutoRules data={await QueryAutoRulesPointsAction()} />;
}
