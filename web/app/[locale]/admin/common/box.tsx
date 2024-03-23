'use client';

import { type ReactNode } from 'react';
import { useRouter } from 'next/navigation';
import Script from 'next/script';
import { useTranslations } from 'next-intl';

export default function Box({
  children,
  header,
  footer,
  onLoadEditor,
  onErrorEditor,
  title,
  hideReturnBtn,
  hideHeader,
}: {
  children: ReactNode;
  footer?: ReactNode;
  header?: ReactNode;
  title?: string;
  onLoadEditor?: () => void;
  onErrorEditor?: (e: any) => void;
  hideReturnBtn?: boolean;
  hideHeader?: boolean;
}) {
  const router = useRouter();
  const t = useTranslations();

  function onClickReturn() {
    router.back();
  }

  return (
    <>
      <div className="card rounded-2">
        {!hideHeader &&
          (header ? (
            <div className="card-header">{header}</div>
          ) : (
            <div className="card-header">
              <div className="d-flex align-items-center justify-content-between gap-4">
                {title ? <div className="fw-bold">{title}</div> : <div></div>}
                <div>
                  {!hideReturnBtn && (
                    <button
                      onClick={onClickReturn}
                      type="button"
                      className="btn btn-sm btn-secondary"
                    >
                      {t('common.return')}
                    </button>
                  )}
                </div>
              </div>
            </div>
          ))}
        <div className="card-body">{children}</div>
        {footer && (
          <div className="card-footer bg-transparent border-top-0">
            {footer}
          </div>
        )}
      </div>

      {typeof onLoadEditor === 'function' &&
        typeof onErrorEditor === 'function' && (
          <Script
            onReady={onLoadEditor}
            onError={onErrorEditor}
            src="/editor/ckeditor.js"
          />
        )}
    </>
  );
}
