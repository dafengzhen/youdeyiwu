import { type Metadata } from 'next';
import Posts from '@/app/posts/posts';
import SelectAllPostAction from '@/app/actions/posts/select-all-post-action';

export const metadata: Metadata = {
  title: 'articles - youdeyiwu',
  description: 'view all article',
};

export default async function Page() {
  return <Posts data={await SelectAllPostAction()} />;
}
