'use server';

import type { IError } from '@/app/[locale]/interfaces';
import type { IPostUser } from '@/app/[locale]/interfaces/posts';
import {
  createErrorResponse,
  createRequest,
  createRequestUrl,
  createSuccessResponse,
} from '@/app/[locale]/common/response';

export default async function QueryUserRelationshipByIdPostAction(variables: {
  id: number | string;
  userId: number | string;
}) {
  try {
    const { url } = createRequestUrl(
      `/posts/${variables.id}/users/${variables.userId}/user-relationship`,
    );
    const response = await createRequest({
      url,
    });

    const data = (await response.json()) as IPostUser | IError;
    if (!response.ok) {
      return createErrorResponse(data);
    }

    return createSuccessResponse(data as IPostUser);
  } catch (e) {
    return createErrorResponse(e);
  }
}
