'use client';

import { useCallback, useEffect } from 'react';
import clsx from 'clsx';
import { useTranslations } from 'next-intl';
import useLocalStorageState from 'use-local-storage-state';

type TColorModeId = 'light' | 'dark' | 'auto';

interface IColorMode {
  id: TColorModeId;
  icon: 'bi-brightness-high-fill' | 'bi-moon-stars-fill' | 'bi-circle-half';
}

const colorModes: IColorMode[] = [
  { id: 'light', icon: 'bi-brightness-high-fill' },
  { id: 'dark', icon: 'bi-moon-stars-fill' },
  { id: 'auto', icon: 'bi-circle-half' },
];

const themeKey = '_youdeyiwu_theme';

export default function ColorModeNavItem() {
  const t = useTranslations();
  const initialValue = { value: 'auto' as TColorModeId };

  const [themeItem, setThemeValue] = useLocalStorageState(themeKey, {
    defaultValue: initialValue,
  });
  const theme = themeItem.value;

  const setTheme = useCallback(
    (value: TColorModeId) => {
      const dataBsTheme = 'data-bs-theme';
      if (
        value === 'auto' &&
        matchMedia('(prefers-color-scheme: dark)').matches
      ) {
        document.documentElement.setAttribute(dataBsTheme, 'dark');
      } else {
        document.documentElement.setAttribute(dataBsTheme, value);
      }
      setThemeValue({ value });
    },
    [setThemeValue],
  );

  useEffect(() => {
    if (theme !== 'auto') {
      setTheme(theme);
    }
  }, [setTheme, theme]);

  useEffect(() => {
    function handleChange() {
      if (theme === 'auto') {
        setTheme(theme);
      }
    }

    matchMedia('(prefers-color-scheme: dark)').addEventListener(
      'change',
      handleChange,
    );

    return () => {
      matchMedia('(prefers-color-scheme: dark)').removeEventListener(
        'change',
        handleChange,
      );
    };
  }, []);

  function onClickColorMode(theme: TColorModeId) {
    setTheme(theme);
  }

  return (
    <li className="nav-item dropstart">
      <a
        className="nav-link"
        role="button"
        data-bs-toggle="dropdown"
        aria-expanded="false"
      >
        {colorModes.map((item) => {
          return (
            theme === item.id && (
              <i key={item.id} className={clsx('bi', item.icon)}></i>
            )
          );
        })}
      </a>

      <ul className="dropdown-menu dropdown-menu-end">
        {colorModes.map((item) => {
          return (
            <li key={item.id}>
              <button
                onClick={() => onClickColorMode(item.id)}
                type="button"
                className={clsx('dropdown-item d-flex align-items-center', {
                  active: theme === item.id,
                })}
              >
                <i className={clsx('bi me-2', item.icon)}></i>
                {t(`common.${item.id}`)}
                {theme === item.id && <i className="bi bi-check2 ms-2"></i>}
              </button>
            </li>
          );
        })}
      </ul>
    </li>
  );
}
