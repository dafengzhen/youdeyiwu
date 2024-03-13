import type { IBase } from '@/app/[locale]/interfaces/index';
import { IUser } from '@/app/[locale]/interfaces/users';
import { IComment } from '@/app/[locale]/interfaces/comments';

export type IReplyReviewState = 'APPROVED' | 'REJECTED' | 'PENDING_REVIEW';

export interface IReply extends IBase {
  content: string;
  likesCount: number;
  reviewState: IReplyReviewState;
  comment?: IComment;
  quoteReply?: IReply;
  user?: IUser;
  uniqueIdentifier?: string;
}
