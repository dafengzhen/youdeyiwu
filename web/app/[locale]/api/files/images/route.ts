import { NextRequest, NextResponse } from 'next/server';
import {
  createErrorResponse,
  createRequest,
  createRequestUrl,
} from '@/app/[locale]/common/response';
import { POST as POST_METHOD } from '@/app/[locale]/constants';
import type { IError } from '@/app/[locale]/interfaces';
import type { IFileUrls } from '@/app/[locale]/interfaces/file';

export async function POST(request: NextRequest) {
  const { url } = createRequestUrl('/files/images');
  const response = await createRequest({
    url,
    options: {
      method: POST_METHOD,
      body: await request.formData(),
      skipBody: true,
      skipHeader: true,
    },
  });

  const data = (await response.json()) as IFileUrls | IError;
  if (!response.ok) {
    return NextResponse.json(createErrorResponse(data));
  }

  return NextResponse.json(data);
}
