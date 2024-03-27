import Link from 'next/link';
import Image from 'next/image';
import { useState } from 'react';
import clsx from 'clsx';
import type { ISectionDetails } from '@/app/[locale]/interfaces/sections';
import { getUserAlias, isHttpOrHttps } from '@/app/[locale]/common/client';
import Content from '@/app/[locale]/components/content/content';
import { useTranslations } from 'next-intl';

export default function Navbar({ details }: { details: ISectionDetails }) {
  const content = details.content ?? '';
  const [continueReading, setContinueReading] = useState(false);
  const t = useTranslations();

  function onClickContinueReading() {
    setContinueReading(!continueReading);
  }

  return (
    <div className="d-flex flex-column gap-4">
      <div className="card yw-card shadow-sm shadow-hover">
        <div className="card-header yw-card-header fw-bold">
          <div className="d-flex align-items-center gap-4 justify-content-between">
            {details.name}
          </div>
        </div>
        <div className="card-body d-flex flex-column gap-3 py-2">
          {details.admins.length > 0 && (
            <div className="d-flex mb-2 align-items-center gap-2">
              <div className="text-secondary">
                <div className="">Admins</div>
                <div className="mt-2 d-flex flex-wrap align-items-center column-gap-2">
                  {details.admins.map((item) => {
                    const avatar = isHttpOrHttps(item.avatar)
                      ? item.avatar!
                      : '/assets/avatar.png';

                    return (
                      <Link
                        key={item.id}
                        href={`/users${item.id}`}
                        data-bs-toggle="tooltip"
                        data-bs-title={getUserAlias(item)}
                      >
                        <Image
                          className="rounded-circle object-fit-contain image-hover"
                          src={avatar}
                          alt="avatar"
                          width={50}
                          height={50}
                        />
                      </Link>
                    );
                  })}
                </div>
              </div>
            </div>
          )}

          <div
            className="overflow-hidden position-relative"
            style={{
              height:
                continueReading || content.length < 600 ? 'auto' : '12rem',
            }}
          >
            {content && (
              <>
                {content && <Content html={content} />}

                {content.length >= 600 && (
                  <>
                    <div style={{ height: '3.75rem' }}></div>
                    <div
                      onClick={onClickContinueReading}
                      className={clsx(
                        'w-100 position-absolute bottom-0 start-0 cursor-pointer',
                        continueReading
                          ? 'animate__animated animate__faster animate__fadeOut'
                          : 'animate__animated animate__faster animate__fadeInUp',
                      )}
                      style={{
                        height: '8rem',
                        background:
                          'linear-gradient(to bottom, rgba(255, 255, 255, 0) 0%, var(--bs-body-bg) 95%)',
                      }}
                    ></div>
                    <button
                      onClick={onClickContinueReading}
                      type="button"
                      className="btn rounded-pill btn-outline-secondary position-absolute bottom-0"
                    >
                      {continueReading
                        ? t('common.collapse')
                        : t('common.expand')}
                    </button>
                  </>
                )}
              </>
            )}
          </div>
        </div>
      </div>
    </div>
  );
}
