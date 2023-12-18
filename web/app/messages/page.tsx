import { type Metadata } from 'next';
import Messages from '@/app/messages/messages';
import QueryAllMessagesAction from '@/app/actions/messages/query-all-messages-action';

export const metadata: Metadata = {
  title: 'messages - youdeyiwu',
  description: 'view all message',
};

export default async function Page() {
  return <Messages data={await QueryAllMessagesAction()} />;
}
