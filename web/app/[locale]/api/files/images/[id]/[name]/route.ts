import { NextRequest, NextResponse } from 'next/server';
import type { IError } from '@/app/[locale]/interfaces';
import {
  createErrorResponse,
  createRequest,
  createRequestUrl,
} from '@/app/[locale]/common/response';

export async function GET(
  request: NextRequest,
  context: { params: { id: string; name: string } },
) {
  const { url, str } = createRequestUrl(
    `/files/images/${context.params.id}/${context.params.name}`,
  );
  const response = await createRequest({
    url,
  });

  if (!response.ok) {
    const data = (await response.json()) as IError;
    return NextResponse.json(createErrorResponse(data));
  }

  return response;
}
