import { type Metadata } from 'next';
import Messages from '@/app/messages/messages';
import QueryAllMessageAction from '@/app/actions/messages/query-all-message-action';
import LoginInfoUserAction from '@/app/actions/users/login-info-user-action';
import ErrorPage from '@/app/common/error-page';

export const metadata: Metadata = {
  title: 'Messages',
};

export default async function Page() {
  const responses = await Promise.all([
    QueryAllMessageAction(),
    LoginInfoUserAction(),
  ]);
  const messageResponse = responses[0];
  const currentUserResponse = responses[1];

  if (messageResponse.isError) {
    return <ErrorPage message={messageResponse.message} />;
  }

  if (currentUserResponse.isError) {
    return <ErrorPage message={currentUserResponse.message} />;
  }

  return (
    <Messages
      data={messageResponse.data}
      currentUser={currentUserResponse.data}
    />
  );
}
