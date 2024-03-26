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
        'card border-0 yw-homepage-bg',
        {
          'border-info': selectedTabIndex === 'MyHomepage',
        },
        isClick ? 'shadow' : 'shadow-hover',
      )}
      onClick={onClickCard}
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
            <div className="text-secondary-emphasis">
              {details.oneSentence
                ? details.oneSentence
                : t('common.heDidntLeaveBehindASingleWord')}
            </div>
          </div>
        </div>
      </div>
    </div>
  );
}
