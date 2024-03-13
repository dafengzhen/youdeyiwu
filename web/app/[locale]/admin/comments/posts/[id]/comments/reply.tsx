import { IComment } from '@/app/[locale]/interfaces/comments';
import { IPostDetails } from '@/app/[locale]/interfaces/posts';
import Link from 'next/link';
import Image from 'next/image';
import { getUserAlias, isHttpOrHttps } from '@/app/[locale]/common/client';
import Content from '@/app/[locale]/components/content/content';

export default function Reply({
  item,
  details,
}: {
  item: IComment;
  details: IPostDetails;
}) {
  const user = item.user;

  return (
    <div className="card border-0 shadow-sm shadow-hover">
      <div className="card-header bg-transparent border-bottom-0 d-flex justify-content-between">
        <div className="d-flex justify-content-around gap-3">
          <Link href={user ? `/users/${user.id}` : '/users'}>
            <Image
              className="rounded-circle object-fit-contain image-hover"
              src={
                user && isHttpOrHttps(user.avatar)
                  ? user.avatar!
                  : '/avatar.png'
              }
              alt="avatar"
              width={50}
              height={50}
            />
          </Link>
          <div className="d-flex flex-column justify-content-around flex-grow-1">
            <div>
              <Link
                className="fw-medium link-body-emphasis link-offset-2 link-underline-opacity-0 link-underline-opacity-100-hover"
                href={user ? `/users/${user.id}` : '/users'}
              >
                {getUserAlias(user)}
              </Link>
            </div>
            <time dateTime={item.createdOn} className="fw-normal small">
              {item.createdOnText}
            </time>
          </div>
        </div>
        <div>
          <Link
            href={`/admin/comments/posts/${details.id}?type=state&cid=${item.id}`}
            className="btn btn-sm btn-outline-secondary"
          >
            <i className="bi bi-gear me-2"></i>
            <span className="">Update State</span>
          </Link>
        </div>
      </div>
      <div className="card-body d-flex flex-column gap-3 py-2">
        <div className="mt-2 overflow-hidden position-relative">
          <Content html={item.content} />
        </div>
      </div>
    </div>
  );
}
