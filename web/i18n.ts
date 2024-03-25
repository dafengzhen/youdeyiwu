import { getRequestConfig } from 'next-intl/server';
import { notFound } from 'next/navigation';

export const defaultLocale = process.env.NEXT_PUBLIC_DEFAULT_LOCALE!;
export const locales = process.env.NEXT_PUBLIC_LOCALES!.split(',');
export const localeNames = process.env.NEXT_PUBLIC_LOCALE_NAMES!.split(',');

export default getRequestConfig(async ({ locale }) => {
  if (!locales.includes(locale)) {
    notFound();
  }

  return {
    messages: (await import(`@/messages/${locale}.json`)).default,
    timeZone: 'UTC',
  };
});
