import { createContext, Dispatch, SetStateAction } from 'react';
import { IPostDetails } from '@/app/interfaces/posts';
import { IUser } from '@/app/interfaces/users';

export const PostIdContext = createContext<{
  details?: IPostDetails;
  currentUser?: IUser | null;
  openReplyBox?: boolean;
  setOpenReplyBox?: Dispatch<SetStateAction<boolean>>;
}>({});
