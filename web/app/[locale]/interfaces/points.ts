import type { IBase } from '@/app/[locale]/interfaces/index';
import type { IUser } from '@/app/[locale]/interfaces/users';

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
  ruleName: RuleNameEnum;
  permissionRuleName: PermissionRuleNameEnum;
  reason?: string;
  user: IUser;
}

export interface IPointRule extends IBase {
  ruleName: RuleNameEnum;
  initiatorRewardPoints: number;
  receiverRewardPoints: number;
  _tip?: string;
}

export interface IPointPermissionRule extends IBase {
  permissionRuleName: PermissionRuleNameEnum;
  requiredPoints: number;
  operationCost: number;
  _tip?: string;
}

export enum RuleNameEnum {
  LIKE_POST = 'LIKE_POST',
  LIKE_COMMENT = 'LIKE_COMMENT',
  LIKE_REPLY = 'LIKE_REPLY',
  COMMENT_POST = 'COMMENT_POST',
  REPLY_POST = 'REPLY_POST',
  // FOLLOW_POST = 'FOLLOW_POST',
  FAVORITE_POST = 'FAVORITE_POST',
  // DISLIKE_POST = 'DISLIKE_POST',
  // DISLIKE_COMMENT = 'DISLIKE_COMMENT',
  // DISLIKE_REPLY = 'DISLIKE_REPLY',
  POST_APPROVED = 'POST_APPROVED',
  POST_NOT_APPROVED = 'POST_NOT_APPROVED',
  POST_PENDING_REVIEW = 'POST_PENDING_REVIEW',
  VISIT_POST = 'VISIT_POST',
  CREATE_POST = 'CREATE_POST',
}

export enum PermissionRuleNameEnum {
  CREATE_POST = 'CREATE_POST',
  CREATE_COMMENT = 'CREATE_COMMENT',
  CREATE_REPLY = 'CREATE_REPLY',
  UPDATE_POST = 'UPDATE_POST',
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
