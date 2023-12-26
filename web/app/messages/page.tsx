import { type Metadata } from 'next';
import Messages from '@/app/messages/messages';
import QueryAllMessageAction from '@/app/actions/messages/query-all-message-action';
import LoginInfoUserAction from '@/app/actions/users/login-info-user-action';

export const metadata: Metadata = {
  title: 'Messages',
};

export default async function Page() {
  return (
    <Messages
      data={await QueryAllMessageAction()}
      currentUser={await LoginInfoUserAction()}
    />
  );
}
