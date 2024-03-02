import { NextRequest } from 'next/server';
import type { IError } from '@/app/interfaces';
import {
  createErrorResponse,
  createRequest,
  createRequestUrl,
} from '@/app/common/response';

export async function GET(
  request: NextRequest,
  context: { params: { id: string } },
) {
  const { url, str } = createRequestUrl(`/posts/${context.params.id}/cover`);
  const response = await createRequest({
    url,
  });

  if (!response.ok) {
    const data = (await response.json()) as IError;
    return createErrorResponse(data);
  }

  return response as any;
}
