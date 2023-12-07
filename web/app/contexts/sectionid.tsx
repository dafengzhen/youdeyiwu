import { createContext } from 'react';
import { IUser } from '@/app/interfaces/users';
import { ISectionDetails } from '@/app/interfaces/sections';

export const SectionIdContext = createContext<{
  details?: ISectionDetails;
  currentUser?: IUser | null;
}>({});
