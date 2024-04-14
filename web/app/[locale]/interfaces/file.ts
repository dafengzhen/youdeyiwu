import type { IBase } from '@/app/[locale]/interfaces/index';
import type { IUser } from '@/app/[locale]/interfaces/users';

export interface IFile extends IBase {
  url: string;
  urls?: IFileUrls;
  name: string;
  originalName: string;
  overview?: string;
  fileCategory: 'IMAGE' | 'ZIP' | 'TEXT';
  storageServiceType: 'DB' | 'OSS';
  businessType:
    | 'SYSTEM'
    | 'SECTION'
    | 'POST'
    | 'TAG'
    | 'COMMENT'
    | 'REPLY'
    | 'MESSAGE'
    | 'USER'
    | 'OTHER';
  contentType: String;
  mediaType: String;
  size: number;
  bucketName?: string;
  objectName?: string;
  viewCount: number;
  digest: string;
  objectKey?: string;
  user?: IUser;
}

export interface IFileUrls {
  url?: string;
  urls?: IFileUrl;
}

export interface IFileUrl {
  default: string;
}
