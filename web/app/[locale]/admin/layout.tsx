import { type ReactNode } from 'react';
import LoginInfoUserAction from '@/app/[locale]/actions/users/login-info-user-action';
import MenusUserAction from '@/app/[locale]/actions/users/menus-user-action';
import { Providers } from '@/app/[locale]/admin/providers';

export default async function RootLayout({
  children,
}: {
  children: ReactNode;
}) {
  let user;
  let menus;
  const responses = await Promise.all([
    LoginInfoUserAction(),
    MenusUserAction(),
  ]);
  const userResponse = responses[0];
  const menuResponse = responses[1];

  if (userResponse.isSuccess) {
    user = userResponse.data;
  }

  if (menuResponse.isSuccess) {
    menus = menuResponse.data;
  }

  return (
    <div className="row mx-0">
      <div className="col">
        <Providers user={user} menus={menus}>
          {children}
        </Providers>
      </div>
    </div>
  );
}
