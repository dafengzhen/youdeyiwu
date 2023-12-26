import Link from 'next/link';
import Image from 'next/image';
import { IUser } from '@/app/interfaces/users';
import { getUserAlias, processFirstCharacter } from '@/app/common/server';
import { IMenu } from '@/app/interfaces/menus';
import { IPage } from '@/app/interfaces';
import { IMessage } from '@/app/interfaces/messages';

export default async function Navbar({
  user,
  menus,
  messages,
}: {
  user: IUser | null;
  menus: IMenu[];
  messages?: IPage<IMessage[]>;
}) {
  let id;
  let avatar;
  let alias = getUserAlias(user);

  if (user) {
    id = user.id;
    avatar = user.avatar;
  }

  return (
    <nav className="navbar navbar-expand-lg bg-body-tertiary sticky-top mb-4">
      <div className="container-fluid">
        <Link href="/" className="navbar-brand">
          <Image
            className="d-inline-block me-2 rounded"
            src="/favicon/android-chrome-512x512.png"
            alt="logo"
            width={40}
            height={40}
          />
          Youdeyiwu
        </Link>
        <div className="collapse navbar-collapse">
          <ul className="navbar-nav me-auto mb-2 mb-lg-0">
            <li className="nav-item">
              <Link className="nav-link" aria-current="page" href="/">
                Home
              </Link>
            </li>
            <li className="nav-item">
              <Link className="nav-link" href="/sections">
                Contents
              </Link>
            </li>
            <li className="nav-item">
              <Link className="nav-link" href="/posts">
                Articles
              </Link>
            </li>

            {messages &&
              messages.content.some((item) => item.state === 'UNREAD') && (
                <li className="nav-item position-relative">
                  <Link className="nav-link" href="/messages">
                    Messages
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
                More
              </a>
              <ul className="dropdown-menu">
                {!(
                  messages &&
                  messages.content.some((item) => item.state === 'UNREAD')
                ) && (
                  <li>
                    <Link className="dropdown-item" href="/messages">
                      Messages
                    </Link>
                  </li>
                )}

                <li>
                  <Link
                    className="dropdown-item"
                    href={id ? `/users/${id}` : '/users'}
                  >
                    My Profile
                  </Link>
                </li>
                <li>
                  <Link className="dropdown-item" href="/posts/new">
                    Create Article
                  </Link>
                </li>

                {menus.length > 0 && (
                  <>
                    <li>
                      <hr className="dropdown-divider" />
                    </li>
                    <li>
                      <Link className="dropdown-item" href="/admin">
                        Dashboard
                      </Link>
                    </li>
                  </>
                )}
              </ul>
            </li>
          </ul>
          <Link
            href={id ? `/users/${id}` : '/users'}
            className="position-relative"
          >
            <Image
              className="rounded-circle object-fit-contain image-hover"
              src={avatar ?? '/avatar.png'}
              alt="avatar"
              width={50}
              height={50}
            />
            <div className="user-select-none text-white position-absolute top-50 start-50 translate-middle d-flex align-items-center justify-content-center fw-medium">
              {processFirstCharacter(alias)}
            </div>
          </Link>
        </div>
      </div>
    </nav>
  );
}
