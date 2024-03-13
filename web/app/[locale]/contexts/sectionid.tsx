import { createContext } from 'react';
import { IUser } from '@/app/[locale]/interfaces/users';
import { ISectionDetails } from '@/app/[locale]/interfaces/sections';

export const SectionIdContext = createContext<{
  details?: ISectionDetails;
  currentUser?: IUser | null;
}>({});
