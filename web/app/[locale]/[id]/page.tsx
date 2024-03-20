import { type Metadata } from 'next';
import UserId from '@/app/[locale]/users/[id]/userid';
import {
  getUserAlias,
  incorrectMetadataTitle,
} from '@/app/[locale]/common/tool';
import QueryDetailsUserAction from '@/app/[locale]/actions/users/query-details-user-action';
import LoginInfoUserAction from '@/app/[locale]/actions/users/login-info-user-action';
import ErrorPage from '@/app/[locale]/common/error-page';

export async function generateMetadata({
  params,
}: {
  params: { id: string };
}): Promise<Metadata> {
  const id = params.id;
  const response = await QueryDetailsUserAction({ id });
  if (response.isError) {
    return incorrectMetadataTitle(response);
  }

  const user = response.data;
  const userAlias = getUserAlias(user);

  const url = process.env.URL + `/users/${user.id}`;
  const newUrl = process.env.URL + `/${id}`;
  const title = userAlias;
  const description = user.oneSentence ?? '';

  return {
    title,
    description,
    alternates: {
      canonical: url,
    },
    authors: {
      url: `/users/${user.id}`,
      name: userAlias,
    },
    creator: `${userAlias}(ID. ${user.id})`,
    bookmarks: newUrl,
    openGraph: {
      url: newUrl,
      title,
      description,
      type: 'profile',
      images: user.avatar
        ? {
            url: user.avatar,
            alt: 'avatar',
          }
        : undefined,
      username: userAlias,
    },
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
