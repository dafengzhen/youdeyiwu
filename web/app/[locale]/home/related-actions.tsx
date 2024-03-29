import Link from 'next/link';
import { useTranslations } from 'next-intl';

export default function RelatedActions({
  isLogin,
  editPostId,
}: {
  isLogin?: boolean;
  editPostId?: number;
}) {
  const t = useTranslations();

  return (
    <div className="card yw-card shadow-sm shadow-hover">
      <div className="card-header yw-card-header fw-bold">
        {t('common.relatedActions')}
      </div>
      <div className="card-body">
        <div className="d-flex flex-column gap-2">
          <Link
            href="/posts/new"
            className="btn rounded-2 btn-primary w-100 d-flex justify-content-center"
          >
            <div
              className="text-start flex-shrink-0 text-truncate"
              style={{ width: 140 }}
            >
              <i className="bi bi-pen me-2"></i>
              {t('common.createArticle')}
            </div>
          </Link>

          {editPostId && (
            <Link
              href={`/posts/${editPostId}/edit`}
              className="btn rounded-2 btn-primary w-100 d-flex justify-content-center"
            >
              <div
                className="text-start flex-shrink-0 text-truncate"
                style={{ width: 140 }}
              >
                <i className="bi bi-pencil-square me-2"></i>
                {t('common.editArticle')}
              </div>
            </Link>
          )}

          {!isLogin && (
            <>
              <Link
                href="/login"
                className="btn rounded-2 btn-primary w-100 d-flex justify-content-center"
              >
                <div
                  className="text-start flex-shrink-0 text-truncate"
                  style={{ width: 140 }}
                >
                  <i className="bi bi-person me-2"></i>
                  {t('common.loginNow')}
                </div>
              </Link>

              <Link
                href="/register"
                className="btn rounded-2 btn-primary w-100 d-flex justify-content-center"
              >
                <div
                  className="text-start flex-shrink-0 text-truncate"
                  style={{ width: 140 }}
                >
                  <i className="bi bi-person-add me-2"></i>
                  {t('common.quickRegister')}
                </div>
              </Link>
            </>
          )}
        </div>
      </div>
    </div>
  );
}
