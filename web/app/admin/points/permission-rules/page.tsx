import { type Metadata } from 'next';
import PointPermissionRules from '@/app/admin/points/permission-rules/point-permission-rules';
import QueryPermissionRulesPointsAction from '@/app/actions/points/permission-rules/query-permission-rules-points-action';

export const metadata: Metadata = {
  title: 'Point Rule',
};

export default async function Page() {
  return (
    <PointPermissionRules data={await QueryPermissionRulesPointsAction()} />
  );
}
