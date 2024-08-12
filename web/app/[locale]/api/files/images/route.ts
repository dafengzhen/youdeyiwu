import { NextRequest, NextResponse } from 'next/server';
import {
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
    return NextResponse.json(data);
  }

  const newData = data as IFileUrls;
  newData.url = processUrl(newData.url);

  const urls = newData.urls;
  if (urls && urls.default) {
    urls.default = <string>processUrl(urls.default);
  }

  return NextResponse.json(newData);
}

const processUrl = (url: string | undefined) => {
  if (url && !(url.startsWith('http') || url.startsWith('https'))) {
    return process.env.URL + url;
  }
  return url;
};
