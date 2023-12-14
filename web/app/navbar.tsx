import Link from 'next/link';
import Image from 'next/image';
import { IUser } from '@/app/interfaces/users';
import { getUserAlias, processFirstCharacter } from '@/app/common/server';
import { IMenu } from '@/app/interfaces/menus';

export default async function Navbar({
  user,
  menus,
}: {
  user: IUser | null;
  menus: IMenu[];
}) {
  let id;
  let avatar;
  let alias = getUserAlias(user);

  if (user) {
    id = user.id;
    avatar = user.avatar;
  }

  return (
    <nav
      className="navbar navbar-expand-lg bg-body-tertiary sticky-top mb-4"
      style={{ height: '3.125rem' }}
    >
      <div className="container-fluid">
        <Link href="/" className="navbar-brand">
          <Image
            className="d-inline-block me-2"
            src="/logo.png"
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
                <li>
                  <Link className="dropdown-item" href="/messages">
                    Messages
                  </Link>
                </li>
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
                <li>
                  <hr className="dropdown-divider" />
                </li>

                {menus.length > 0 && (
                  <li>
                    <Link className="dropdown-item" href="/admin">
                      Dashboard
                    </Link>
                  </li>
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
