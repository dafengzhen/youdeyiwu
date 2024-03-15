import type { TTabId } from '@/app/[locale]/users/[id]/userid';
import clsx from 'clsx';
import Image from 'next/image';
import { useState } from 'react';
import type { IUserDetails } from '@/app/[locale]/interfaces/users';
import { getUserAlias, isHttpOrHttps } from '@/app/[locale]/common/client';
import { useTranslations } from 'next-intl';

export default function MyHomepage({
  selectedTabIndex,
  details,
}: {
  selectedTabIndex?: TTabId;
  details: IUserDetails;
}) {
  const [isClick, setIsClick] = useState(false);
  const avatar = details.avatar;
  const t = useTranslations();

  function onClickCard() {
    setIsClick(!isClick);
  }

  return (
    <div
      className={clsx(
        'card border-0',
        {
          'border-info': selectedTabIndex === 'MyHomepage',
        },
        isClick ? 'shadow' : 'shadow-hover',
      )}
      onClick={onClickCard}
      style={{
        backgroundImage:
          'linear-gradient(91.7deg, rgba(252, 190, 224, 1) 16.8%, rgba(212, 254, 218, 1) 103.6%)',
      }}
    >
      <div className="card-body">
        <div className="d-flex gap-3">
          <Image
            className="rounded-circle object-fit-contain"
            src={isHttpOrHttps(avatar) ? avatar! : '/avatar.png'}
            alt="avatar"
            width={80}
            height={80}
          />
          <div className="flex-grow-1 d-flex flex-column justify-content-around">
            <div className="fw-bold user-select-all">
              {`${getUserAlias(details)} (ID. ${details.id})`}
            </div>
            <div>
              {details.oneSentence ? (
                details.oneSentence
              ) : (
                <span>{t('common.heDidntLeaveBehindASingleWord')}</span>
              )}
            </div>
          </div>
        </div>
      </div>
    </div>
  );
}
