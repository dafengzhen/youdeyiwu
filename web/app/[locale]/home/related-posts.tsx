import type { IPost } from '@/app/[locale]/interfaces/posts';
import Nodata from '@/app/[locale]/common/nodata';
import Link from 'next/link';
import { useTranslations } from 'next-intl';

export default function RelatedPosts({
  randomData = [],
}: {
  randomData?: IPost[];
}) {
  const t = useTranslations();

  return (
    <>
      {randomData.length > 0 && (
        <div className="card border-0 shadow-sm shadow-hover">
          <div className="card-header bg-transparent border-bottom-0 fw-bold">
            {t('common.relatedArticles')}
          </div>
          <div className="card-body p-0">
            {randomData.map((item) => {
              return (
                <div
                  key={item.id}
                  className="card border-0 cursor-pointer card-hover"
                >
                  <Link
                    className="link-body-emphasis text-decoration-none"
                    href={`/posts/${item.id}`}
                    scroll={false}
                  >
                    <div className="card-body py-2">
                      <div className="line-clamp-2">{item.name}</div>
                    </div>
                  </Link>
                </div>
              );
            })}

            {randomData.length === 0 && <Nodata />}
          </div>
        </div>
      )}
    </>
  );
}
