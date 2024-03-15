import type { TTabId } from '@/app/[locale]/users/[id]/userid';
import clsx from 'clsx';
import type { IUserDetails } from '@/app/[locale]/interfaces/users';
import { useTranslations } from 'next-intl';

export default function RelatedStatistics({
  selectedTabIndex,
  details,
}: {
  selectedTabIndex?: TTabId;
  details: IUserDetails;
}) {
  const {
    sections = 0,
    tags = 0,
    posts = 0,
    comments = 0,
    replies = 0,
    views = 0,
  } = details.relatedStatistics ?? {};
  const t = useTranslations();

  return (
    <div
      className={clsx('card', {
        'border-info': selectedTabIndex === 'RelatedStatistics',
      })}
    >
      <div className="card-body">
        <div className="row row-cols-6 g-4">
          <div className="col">
            <div className="card border-0">
              <div className="card-body text-center">
                <div className="">
                  <i className="bi bi-bar-chart me-2"></i>
                  <span className="">{t('common.contents')}</span>
                </div>
                <div className="card-title h5">{sections}</div>
              </div>
            </div>
          </div>

          <div className="col">
            <div className="card border-0">
              <div className="card-body text-center">
                <div className="">
                  <i className="bi bi-bar-chart me-2"></i>
                  <span className="">{t('common.tags')}</span>
                </div>
                <div className="card-title h5">{tags}</div>
              </div>
            </div>
          </div>

          <div className="col">
            <div className="card border-0">
              <div className="card-body text-center">
                <div className="">
                  <i className="bi bi-bar-chart me-2"></i>
                  <span className="">{t('common.articles')}</span>
                </div>
                <div className="card-title h5">{posts}</div>
              </div>
            </div>
          </div>

          <div className="col">
            <div className="card border-0">
              <div className="card-body text-center">
                <div className="">
                  <i className="bi bi-bar-chart me-2"></i>
                  <span className="">{t('common.comments')}</span>
                </div>
                <div className="card-title h5">{comments}</div>
              </div>
            </div>
          </div>

          <div className="col">
            <div className="card border-0">
              <div className="card-body text-center">
                <div className="">
                  <i className="bi bi-bar-chart me-2"></i>
                  <span className="">{t('common.replies')}</span>
                </div>
                <div className="card-title h5">{replies}</div>
              </div>
            </div>
          </div>

          <div className="col">
            <div className="card border-0">
              <div className="card-body text-center">
                <div className="">
                  <i className="bi bi-bar-chart me-2"></i>
                  <span className="">{t('common.views')}</span>
                </div>
                <div className="card-title h5">{views}</div>
              </div>
            </div>
          </div>
        </div>
      </div>
    </div>
  );
}
