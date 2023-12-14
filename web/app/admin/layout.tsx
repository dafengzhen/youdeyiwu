import React, { type ReactNode } from 'react';
import LoginInfoUserAction from '@/app/actions/users/login-info-user-action';
import MenusUserAction from '@/app/actions/users/menus-user-action';
import { Providers } from '@/app/admin/providers';

export default async function RootLayout({
  children,
}: {
  children: ReactNode;
}) {
  const user = await LoginInfoUserAction();
  const menus = await MenusUserAction();

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
