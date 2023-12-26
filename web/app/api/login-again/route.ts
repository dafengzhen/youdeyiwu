import { NextRequest } from 'next/server';
import { clearCredentials } from '@/app/common/server';
import { redirect } from 'next/navigation';

export async function GET(request: NextRequest) {
  clearCredentials();
  redirect('/login');
}
