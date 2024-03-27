import { IReply } from '@/app/[locale]/interfaces/replies';
import { IPostDetails } from '@/app/[locale]/interfaces/posts';
import Link from 'next/link';
import Image from 'next/image';
import { getUserAlias, isHttpOrHttps } from '@/app/[locale]/common/client';
import Content from '@/app/[locale]/components/content/content';
import { useTranslations } from 'next-intl';

export default function QuotedReply({
  item,
  details,
}: {
  item: IReply;
  details: IPostDetails;
}) {
  const user = item.user;
  const quotedItem = item.quoteReply ?? item.comment;
  const t = useTranslations();

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
                  : '/assets/avatar.png'
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
            href={`/admin/comments/posts/${details.id}?type=state&rid=${item.id}`}
            className="btn btn-sm btn-outline-secondary"
          >
            <i className="bi bi-gear me-2"></i>
            <span className="">{t('common.updateState')}</span>
          </Link>
        </div>
      </div>
      <div className="card-body d-flex flex-column gap-3 py-2">
        {quotedItem && (
          <div className="mt-2 border-start border-0 border-3 ps-3">
            <figure>
              <blockquote className="blockquote fs-6 mb-2">
                <Content html={quotedItem.content} />
              </blockquote>
              <figcaption className="blockquote-footer mt-0">
                <cite title="Source Title">
                  <Link
                    className="link-body-emphasis link-offset-2 link-underline-opacity-0 link-underline-opacity-100-hover"
                    href={
                      quotedItem.user
                        ? `/users/${quotedItem.user.id}`
                        : '/users'
                    }
                  >
                    {getUserAlias(quotedItem.user)}
                  </Link>
                  <span className="mx-1">·</span>
                  <time dateTime={quotedItem.createdOn} className="">
                    {quotedItem.createdOnText}
                  </time>
                </cite>
              </figcaption>
            </figure>
          </div>
        )}

        <div className="overflow-hidden position-relative">
          <div
            dangerouslySetInnerHTML={{
              __html: item.content,
            }}
          ></div>
        </div>
      </div>
    </div>
  );
}
