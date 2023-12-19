import { IBase } from '@/app/interfaces/index';
import { IUser } from '@/app/interfaces/users';

export interface IMessage extends IBase {
  name: string;
  overview: string;
  content?: Record<string, any>;
  messageType: TMessageType;
  messageRange: TMessageRange;
  state?: TMessageState;
  sender?: IUser;
  receiver?: IUser;
  link?: string;
}

export interface IGlobalMessage extends Omit<IMessage, 'receiver'> {
  sort: number;
}

export type TMessageType = keyof typeof MessageTypeEnum;

export type TMessageRange = keyof typeof MessageRangeEnum;

export type TMessageState = keyof typeof MessageStateEnum;

enum MessageTypeEnum {
  SYSTEM = 'SYSTEM',
  GLOBAL_MESSAGE = 'GLOBAL_MESSAGE',
  MESSAGE = 'MESSAGE',
  CONFIG = 'CONFIG',
  FILE = 'FILE',
  USER = 'USER',
  ROLE = 'ROLE',
  PERMISSION = 'PERMISSION',
  MENU = 'MENU',
  SUBMENU = 'SUBMENU',
  ACTION = 'ACTION',
  SECTION = 'SECTION',
  SECTION_GROUP = 'SECTION_GROUP',
  POST = 'POST',
  TAG = 'TAG',
  TAG_GROUP = 'TAG_GROUP',
  COMMENT = 'COMMENT',
  REPLY = 'REPLY',
  POINT = 'POINT',
}

enum MessageRangeEnum {
  ALL_USER = 'ALL_USER',
  USER = 'USER',
}

enum MessageStateEnum {
  UNREAD = 'UNREAD',
  READ = 'READ',
}
