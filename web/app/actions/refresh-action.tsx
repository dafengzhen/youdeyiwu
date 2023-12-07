'use server';

import { revalidateTag } from 'next/cache';
import { redirect, RedirectType } from 'next/navigation';

export default async function RefreshAction(variables: {
  tags: string[];
  url?: string;
}) {
  variables.tags.forEach((tag) => {
    revalidateTag(tag);
  });

  if (variables.url) {
    redirect(variables.url, RedirectType.replace);
  }
}
