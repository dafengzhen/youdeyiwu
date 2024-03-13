import { type Metadata } from 'next';
import UserId from '@/app/[locale]/users/[id]/userid';
import {
  getUserAlias,
  incorrectMetadataTitle,
  isNum,
} from '@/app/[locale]/common/tool';
import { notFound } from 'next/navigation';
import QueryDetailsUserAction from '@/app/[locale]/actions/users/query-details-user-action';
import LoginInfoUserAction from '@/app/[locale]/actions/users/login-info-user-action';
import ErrorPage from '@/app/[locale]/common/error-page';

export async function generateMetadata({
  params,
}: {
  params: { id: string };
}): Promise<Metadata> {
  const id = params.id;
  if (!isNum(id)) {
    notFound();
  }

  const response = await QueryDetailsUserAction({ id });
  if (response.isError) {
    return incorrectMetadataTitle(response);
  }

  const user = response.data;
  const userAlias = getUserAlias(user);

  return {
    title: userAlias,
    authors: {
      url: `/users/${user.id}`,
      name: userAlias,
    },
    creator: `${user}(ID. ${user.id})`,
    description: user.oneSentence ?? '',
    bookmarks: `/users/${user.id}`,
  };
}

export default async function Page({
  params,
}: {
  params: {
    id: string;
  };
}) {
  const id = params.id;
  if (!isNum(id)) {
    notFound();
  }

  const responses = await Promise.all([
    QueryDetailsUserAction({ id }),
    LoginInfoUserAction(),
  ]);
  const userResponse = responses[0];
  const currentUserResponse = responses[1];

  if (userResponse.isError) {
    return <ErrorPage message={userResponse.message} />;
  }

  if (currentUserResponse.isError) {
    return <ErrorPage message={currentUserResponse.message} />;
  }

  return (
    <UserId
      details={userResponse.data}
      currentUser={currentUserResponse.data}
    />
  );
}
