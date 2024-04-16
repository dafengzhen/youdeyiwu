import Link from 'next/link';
import Image from 'next/image';
import type { IUser } from '@/app/[locale]/interfaces/users';
import type { IMenu } from '@/app/[locale]/interfaces/menus';
import type { IPage } from '@/app/[locale]/interfaces';
import type { IMessage } from '@/app/[locale]/interfaces/messages';
import { getTranslations } from 'next-intl/server';
import TranslateNavItem from '@/app/[locale]/common/translate';
import ColorModeNavItem from '@/app/[locale]/common/color-mode';
import { BLUR_DATA_URL } from '@/app/[locale]/constants';

export default async function Navbar({
  locale,
  user,
  menus,
  messages,
}: {
  locale: string;
  user: IUser | null | undefined;
  menus: IMenu[] | null | undefined;
  messages?: IPage<IMessage[]>;
}) {
  let id;
  const t = await getTranslations({ locale });
  const showLanguageSwitcher = process.env.SHOW_LANGUAGE_SWITCHER === 'true';

  if (user) {
    id = user.id;
  }

  return (
    <nav className="navbar navbar-expand-lg bg-body-tertiary sticky-top mb-4">
      <div className="container-fluid">
        <Link href="/" className="navbar-brand" scroll={false}>
          <Image
            className="d-inline-block me-2 rounded"
            src="/assets/favicon/android-chrome-512x512.png"
            alt="logo"
            width={40}
            height={40}
            placeholder="blur"
            blurDataURL={BLUR_DATA_URL}
          />
          {process.env.NAME}
        </Link>
        <div className="collapse navbar-collapse gap-2">
          <ul className="navbar-nav me-auto mb-lg-0">
            <li className="nav-item">
              <Link
                className="nav-link"
                aria-current="page"
                href="/"
                scroll={false}
              >
                {t('common.home')}
              </Link>
            </li>
            <li className="nav-item">
              <Link className="nav-link" href="/sections" scroll={false}>
                {t('common.contents')}
              </Link>
            </li>
            <li className="nav-item">
              <Link className="nav-link" href="/posts" scroll={false}>
                {t('common.articles')}
              </Link>
            </li>

            {messages &&
              messages.content.some((item) => item.state === 'UNREAD') && (
                <li className="nav-item position-relative">
                  <Link className="nav-link" href="/messages">
                    {t('common.messages')}
                  </Link>
                  <span
                    className="position-absolute translate-middle p-1 bg-danger border border-light rounded-circle"
                    style={{ top: '0.8rem', right: '-0.35rem' }}
                  >
                    <span className="visually-hidden">New messages</span>
                  </span>
                </li>
              )}

            <li className="nav-item dropdown">
              <a
                className="nav-link dropdown-toggle"
                href="#"
                role="button"
                data-bs-toggle="dropdown"
                aria-expanded="false"
              >
                {t('common.more')}
              </a>
              <ul className="dropdown-menu">
                {!(
                  messages &&
                  messages.content.some((item) => item.state === 'UNREAD')
                ) && (
                  <li>
                    <Link className="dropdown-item" href="/messages">
                      {t('common.messages')}
                    </Link>
                  </li>
                )}

                <li>
                  <Link
                    className="dropdown-item text-capitalize"
                    href={id ? `/users/${id}` : '/users'}
                  >
                    {t('common.myProfile')}
                  </Link>
                </li>
                <li>
                  <Link className="dropdown-item" href="/posts/new">
                    {t('common.createArticle')}
                  </Link>
                </li>

                {!user && (
                  <>
                    <li>
                      <Link className="dropdown-item" href="/login">
                        {t('common.loginNow')}
                      </Link>
                    </li>
                    <li>
                      <Link className="dropdown-item" href="/register">
                        {t('common.quickRegister')}
                      </Link>
                    </li>
                  </>
                )}

                {menus && !!menus.length && (
                  <>
                    <li>
                      <hr className="dropdown-divider" />
                    </li>
                    <li>
                      <Link className="dropdown-item" href="/admin">
                        {t('common.dashboard')}
                      </Link>
                    </li>
                  </>
                )}
              </ul>
            </li>
          </ul>
          <ul className="navbar-nav align-items-center">
            {showLanguageSwitcher && (
              <>
                <TranslateNavItem />

                <li className="nav-item">
                  <div className="vr d-flex h-100 mx-2"></div>
                </li>
              </>
            )}

            <ColorModeNavItem />
          </ul>
        </div>
      </div>
    </nav>
  );
}
