'use server';

import type { IError } from '@/app/[locale]/interfaces';
import type { IUser } from '@/app/[locale]/interfaces/users';
import {
  createErrorResponse,
  createRequest,
  createRequestUrl,
  createSuccessResponse,
} from '@/app/[locale]/common/response';

export default async function LoginInfoUserAction() {
  try {
    const { url } = createRequestUrl('/users/login-info');
    const response = await createRequest({
      url,
    });

    if (!response.ok) {
      const data = (await response.json()) as IError;
      return createErrorResponse(data);
    }

    const arrayBuffer = await response.arrayBuffer();
    return createSuccessResponse(
      arrayBuffer.byteLength === 0
        ? null
        : (JSON.parse(new TextDecoder().decode(arrayBuffer)) as IUser),
    );
  } catch (e) {
    return createErrorResponse(e);
  }
}
