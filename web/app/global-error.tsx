'use client';

import ErrorHandler from '@/app/common/error-handler';

export default function GlobalError({
  error,
  reset,
}: {
  error: Error & { digest?: string };
  reset: () => void;
}) {
  return <ErrorHandler error={error} reset={reset} />;
}
