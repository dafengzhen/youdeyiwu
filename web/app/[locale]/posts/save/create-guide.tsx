import { useTranslations } from 'next-intl';

export default function CreateGuide({ data }: { data?: string }) {
  const t = useTranslations();

  return (
    data && (
      <div className="accordion mb-4">
        <div className="accordion-item">
          <h2 className="accordion-header">
            <button
              className="accordion-button"
              type="button"
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
            className="accordion-collapse collapse show"
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
