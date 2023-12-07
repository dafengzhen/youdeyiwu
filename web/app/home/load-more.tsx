import clsx from 'clsx';

export default function LoadMore({
  className,
  onCLickLoadMore,
  isLoading,
}: {
  className?: string;
  onCLickLoadMore?: () => void;
  isLoading?: boolean;
}) {
  return (
    <button
      onClick={onCLickLoadMore}
      type="button"
      className={clsx('btn btn-sm btn-hover rounded-pill', className)}
      title="Load more"
    >
      {isLoading ? 'Loading' : 'Load More'}
    </button>
  );
}
