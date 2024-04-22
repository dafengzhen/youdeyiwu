'use server';

import { type IError } from '@/app/[locale]/interfaces';
import {
  createErrorResponse,
  createRequest,
  createRequestUrl,
  createSuccessResponse,
} from '@/app/[locale]/common/response';
import { ITagGroup } from '@/app/[locale]/interfaces/tag-groups';

export default async function SelectAllTagGroupAction() {
  try {
    const { url } = createRequestUrl('/tag-groups/select-all');
    const response = await createRequest({
      url,
      options: {
        next: {
          tags: ['/tag-groups/select-all'],
        },
      },
    });

    const data = (await response.json()) as ITagGroup[] | IError;
    if (!response.ok) {
      return createErrorResponse(data);
    }

    return createSuccessResponse(data as ITagGroup[]);
  } catch (e) {
    return createErrorResponse(e);
  }
}
