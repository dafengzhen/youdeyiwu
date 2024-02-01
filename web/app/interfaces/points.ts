import type { IBase } from '@/app/interfaces/index';
import { IUser } from '@/app/interfaces/users';

export interface IPoint extends IBase {
  points: number;
  minPoints: number;
  maxPoints: number;
  user: IUser;
}

export interface IPointHistory extends IBase {
  pointValue: number;
  sign: SignEnum;
  points: number;
  minPoints: number;
  maxPoints: number;
  autoRuleName: AutoRuleNameEnum;
  ruleName: RuleNameEnum;
  reason?: string;
  user: IUser;
}

export interface IPointAutoRule extends IBase {
  autoRuleName: AutoRuleNameEnum;
  requiredPoints: number;
  _tip?: string;
}

export interface IPointRule extends IBase {
  ruleName: RuleNameEnum;
  requiredPoints: number;
  _tip?: string;
}

export enum AutoRuleNameEnum {
  LIKED_YOUR_POST = 'LIKED_YOUR_POST',
  LIKED_YOUR_COMMENT = 'LIKED_YOUR_COMMENT',
  LIKED_YOUR_REPLY = 'LIKED_YOUR_REPLY',
  COMMENTED_ON_YOUR_POST = 'COMMENTED_ON_YOUR_POST',
  REPLIED_TO_YOUR_POST = 'REPLIED_TO_YOUR_POST',
  FOLLOWED_YOUR_POST = 'FOLLOWED_YOUR_POST',
  BOOKMARKED_YOUR_POST = 'BOOKMARKED_YOUR_POST',
  APPRECIATED_YOUR_POST = 'APPRECIATED_YOUR_POST',
  DISLIKED_YOUR_POST = 'DISLIKED_YOUR_POST',
  DISLIKED_YOUR_COMMENT = 'DISLIKED_YOUR_COMMENT',
  DISLIKED_YOUR_REPLY = 'DISLIKED_YOUR_REPLY',
  POST_NOT_APPROVED = 'POST_NOT_APPROVED',
  POST_UNDER_REVIEW = 'POST_UNDER_REVIEW',
  VISITED_YOUR_POST = 'VISITED_YOUR_POST',
}

export enum RuleNameEnum {
  CREATE_POST = 'CREATE_POST',
  CREATE_COMMENT = 'CREATE_COMMENT',
  LIKE_POST = 'LIKE_POST',
  LIKE_COMMENT = 'LIKE_COMMENT',
  UPDATE_POST = 'UPDATE_POST',
  FOLLOW_POST = 'FOLLOW_POST',
  FAVORITE_POST = 'FAVORITE_POST',
  CREATE_REPLY = 'CREATE_REPLY',
  ADD_POST_TAG = 'ADD_POST_TAG',
  ADD_POST_CONTENT_LINK = 'ADD_POST_CONTENT_LINK',
  ADD_POST_COVER_LINK = 'ADD_POST_COVER_LINK',
  ADD_POST_SECTION = 'ADD_POST_SECTION',
}

export enum SignEnum {
  POSITIVE = 'POSITIVE',
  NEGATIVE = 'NEGATIVE',
  ZERO = 'ZERO',
}
