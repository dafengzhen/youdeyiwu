import { type Metadata } from 'next';
import Messages from '@/app/messages/messages';

export const metadata: Metadata = {
  title: 'messages - youdeyiwu',
  description: 'view all message',
};

export default async function Page() {
  return <Messages />;
}
