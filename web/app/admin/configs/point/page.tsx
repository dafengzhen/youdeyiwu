import { type Metadata } from 'next';
import PointConfig from '@/app/admin/configs/point/point-config';
import QueryPointConfigAction from '@/app/actions/configs/point/query-point-config-action';

export const metadata: Metadata = {
  title: 'Point Config',
};

export default async function Page() {
  return <PointConfig config={await QueryPointConfigAction()} />;
}
