import type { IBase } from '@/app/[locale]/interfaces/index';
import { IUser } from '@/app/[locale]/interfaces/users';

export type ICommentReviewState = 'APPROVED' | 'REJECTED' | 'PENDING_REVIEW';

export interface IComment extends IBase {
  content: string;
  likesCount: number;
  reviewState: ICommentReviewState;
  user?: IUser;
  uniqueIdentifier?: string;
}
