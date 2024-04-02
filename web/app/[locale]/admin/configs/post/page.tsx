import { type Metadata } from 'next';
import PostConfig from '@/app/[locale]/admin/configs/post/post-config';

export const metadata: Metadata = {
  title: 'Post Config',
};

export default async function Page() {
  return <PostConfig />;
}
