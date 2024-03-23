import Link from 'next/link';
import Image from 'next/image';
import { useState } from 'react';
import clsx from 'clsx';
import { IPostDetails } from '@/app/[locale]/interfaces/posts';
import {
  convertToCamelCase,
  fromNow,
  getUserAlias,
  isHttpOrHttps,
} from '@/app/[locale]/common/client';
import Nodata from '@/app/[locale]/common/nodata';
import Content from '@/app/[locale]/components/content/content';
import { useTranslations } from 'next-intl';

export default function Navbar({ details }: { details: IPostDetails }) {
  const user = details.user;
  const content = details.content ?? '';
  let uid;
  let avatar;
  let alias = getUserAlias(user);

  const [continueReading, setContinueReading] = useState(false);
  const t = useTranslations();

  if (user) {
    uid = user.id;
    avatar = isHttpOrHttps(user.avatar) ? user.avatar : undefined;
  }

  function onClickContinueReading() {
    setContinueReading(!continueReading);
  }

  return (
    <div className="d-flex flex-column gap-4">
      <div className="card border-0 shadow-sm shadow-hover">
        <div className="card-header bg-transparent border-bottom-0 fw-bold">
          <div className="d-flex justify-content-around gap-3">
            <Link href={uid ? `/users/${uid}` : '/users'}>
              <Image
                className="rounded-circle object-fit-contain image-hover"
                src={avatar ? avatar : '/avatar.png'}
                alt="avatar"
                width={50}
                height={50}
              />
            </Link>
            <div className="flex-grow-1 d-flex flex-column justify-content-around">
              <div className="d-flex align-items-center gap-4 justify-content-between">
                <div>
                  <Link
                    className="link-body-emphasis link-offset-2 link-underline-opacity-0 link-underline-opacity-100-hover"
                    href={`/posts/${details.id}`}
                  >
                    {details.name}
                  </Link>
                </div>

                {details.reviewState !== 'APPROVED' && (
                  <div className="badge rounded-pill text-bg-dark text-capitalize">
                    {convertToCamelCase(details.reviewState)}
                  </div>
                )}
              </div>
              <div className="d-flex gap-2 small">
                <div>
                  <Link
                    className="fw-medium text-truncate link-dark link-underline-opacity-0 link-underline-opacity-100-hover link-offset-2"
                    href={uid ? `/users/${uid}` : '/users'}
                  >
                    {alias}
                  </Link>
                </div>
                <time className="fw-normal">{fromNow(details.createdOn)}</time>
              </div>
            </div>
          </div>
        </div>
        <div className="card-body d-flex flex-column gap-3 py-2">
          <div
            className="mt-2 overflow-hidden position-relative"
            style={{
              height:
                continueReading || content.length < 600 ? 'auto' : '12rem',
            }}
          >
            {content ? <Content html={content} /> : <Nodata />}

            {content.length > 600 && (
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
                    backgroundImage:
                      'linear-gradient(to bottom, rgba(255, 255, 255, 0) 0%, #fff 95%)',
                  }}
                ></div>
                <button
                  onClick={onClickContinueReading}
                  type="button"
                  className="btn rounded-pill btn-outline-secondary position-absolute bottom-0"
                >
                  {continueReading
                    ? t('common.collapse')
                    : t('common.expandTheArticle')}
                </button>
              </>
            )}
          </div>
        </div>
      </div>
    </div>
  );
}
