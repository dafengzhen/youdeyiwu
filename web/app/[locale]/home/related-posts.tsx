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
        <div className="card yw-card shadow-sm shadow-hover">
          <div className="card-header yw-card-header fw-bold">
            {t('common.relatedArticles')}
          </div>
          <div className="card-body p-0">
            {randomData.map((item) => {
              return (
                <div
                  key={item.id}
                  className="card border-0 cursor-pointer card-hover"
                >
                  <div className="card-body py-2">
                    <Link
                      className="link-body-emphasis text-decoration-none"
                      href={`/posts/${item.id}`}
                      scroll={false}
                    >
                      <div className="line-clamp-2">{item.name}</div>
                    </Link>
                  </div>
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
