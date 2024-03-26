import clsx from 'clsx';
import { useTranslations } from 'next-intl';

export default function LoadMore({
  className,
  onCLickLoadMore,
  isLoading,
}: {
  className?: string;
  onCLickLoadMore?: () => void;
  isLoading?: boolean;
}) {
  const t = useTranslations();

  return (
    <button
      onClick={onCLickLoadMore}
      type="button"
      className={clsx(
        'btn btn-sm btn-hover text-body-secondary rounded-pill',
        className,
      )}
      title="Load more"
    >
      {isLoading ? t('common.loading') : t('common.loadMore')}
    </button>
  );
}
