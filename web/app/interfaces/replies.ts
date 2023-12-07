import type { IBase } from '@/app/interfaces/index';
import { IUser } from '@/app/interfaces/users';
import { IComment } from '@/app/interfaces/comments';

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
