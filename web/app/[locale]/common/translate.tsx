'use client';

import { useLocale } from 'use-intl';
import { useTransition } from 'react';
import clsx from 'clsx';
import { localeNames, locales } from '@/i18n';

export default function TranslateNavItem() {
  const locale = useLocale();
  const [isPending, startTransition] = useTransition();

  function onClickTranslate(value: string) {
    const url =
      location.origin +
      `/${value}` +
      location.pathname +
      location.search +
      location.hash;
    startTransition(() => {
      location.replace(url);
    });
  }

  return (
    <li className="nav-item dropstart">
      <a
        className="nav-link"
        role="button"
        data-bs-toggle="dropdown"
        aria-expanded="false"
      >
        <i className="bi bi-translate"></i>
      </a>

      <ul className="dropdown-menu dropdown-menu-end">
        {localeNames.map((item, index) => {
          const localeItem = locales[index];

          return (
            <li key={item}>
              <button
                disabled={isPending}
                onClick={() => onClickTranslate(localeItem)}
                type="button"
                className={clsx('dropdown-item d-flex align-items-center', {
                  active: locale === localeItem,
                })}
              >
                {isPending && (
                  <span
                    className="spinner-border spinner-border-sm me-2"
                    role="status"
                    aria-hidden="true"
                  ></span>
                )}
                {item}
                {locale === localeItem && <i className="bi bi-check2 ms-2"></i>}
              </button>
            </li>
          );
        })}
      </ul>
    </li>
  );
}
