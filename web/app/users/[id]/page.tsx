import { type Metadata } from 'next';
import UserId from '@/app/users/[id]/userid';
import {
  errorContent,
  errorTitle,
  getUserAlias,
  isNum,
} from '@/app/common/server';
import { notFound } from 'next/navigation';
import QueryDetailsUserAction from '@/app/actions/users/query-details-user-action';
import LoginInfoUserAction from '@/app/actions/users/login-info-user-action';
import ClientErrorHandler from '@/app/common/client-error-handler';
import { IUserDetails } from '@/app/interfaces/users';

export async function generateMetadata({
  params,
}: {
  params: { id: string };
}): Promise<Metadata> {
  const id = params.id;
  if (!isNum(id)) {
    notFound();
  }

  let details: IUserDetails;
  try {
    details = await QueryDetailsUserAction({ id });
  } catch (e) {
    return errorTitle(e);
  }

  const user = details;
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

  try {
    return (
      <UserId
        details={await QueryDetailsUserAction({ id })}
        currentUser={await LoginInfoUserAction()}
      />
    );
  } catch (e) {
    return <ClientErrorHandler message={errorContent(e)} />;
  }
}
