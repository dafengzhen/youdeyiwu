import { useTranslations } from 'next-intl';
import useLocalStorageState from 'use-local-storage-state';
import clsx from 'clsx';

const openPostCreateGuideKey = '_youdeyiwu_post_create_guide_open';

export default function CreateGuide({ data }: { data?: string }) {
  const t = useTranslations();
  const [openPostCreateGuide, setOpenPostCreateGuide] = useLocalStorageState(
    openPostCreateGuideKey,
    {
      defaultValue: true,
    },
  );

  function onClick() {
    setOpenPostCreateGuide(!openPostCreateGuide);
  }

  return (
    data && (
      <div className="accordion mb-4">
        <div className="accordion-item">
          <h2 className="accordion-header">
            <button
              className="accordion-button"
              type="button"
              onClick={onClick}
              data-bs-toggle="collapse"
              data-bs-target="#yw-collapse-post-create-guide"
              aria-expanded="true"
              aria-controls="collapse createGuide"
            >
              {t('common.userGuide')}
            </button>
          </h2>
          <div
            id="yw-collapse-post-create-guide"
            className={clsx('accordion-collapse collapse', {
              show: openPostCreateGuide,
            })}
          >
            <div
              className="accordion-body"
              dangerouslySetInnerHTML={{ __html: data }}
            />
          </div>
        </div>
      </div>
    )
  );
}
