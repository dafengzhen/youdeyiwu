import { NextRequest } from 'next/server';
import { redirect } from 'next/navigation';
import { cookies } from 'next/headers';
import { SECURE_TK, TK } from '@/app/constants';

export async function GET(request: NextRequest) {
  clearCredentials();
  redirect('/login');
}

const clearCredentials = () => {
  const isHttpsSite = process.env.IS_HTTPS_SITE === 'true';
  cookies().set(isHttpsSite ? SECURE_TK : TK, '', {
    path: '/',
    maxAge: 0,
  });
};
