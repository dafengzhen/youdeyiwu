import Link from 'next/link';

export default function ErrorHandler({
  error,
  reset,
}: {
  error: Error & { digest?: string };
  reset: () => void;
}) {
  if (!error.message) {
    console.error(error);
  }

  return (
    <div className="row mx-0">
      <div className="col">
        <div className="d-flex justify-content-center align-items-center min-h-screen">
          <div className="card border-0 text-center">
            <div className="card-body text-danger">
              <h5 className="card-title text-5xl font-bold">
                Sorry, an error has occurred
              </h5>
              <div className="card-subtitle my-4 d-flex flex-wrap flex-column gap-2 user-select-all cursor-copy">
                {error.message ? (
                  <div className="">
                    <span>
                      === <span className="fw-bold">Details</span> ===
                    </span>
                    &nbsp;
                    <span>{error.message}</span>
                  </div>
                ) : (
                  <div className="">Unknown Error</div>
                )}

                {error.digest && (
                  <div className="">
                    <span>
                      === <span className="fw-bold">Digest</span> ===
                    </span>
                    &nbsp;
                    <span>{error.digest}</span>
                  </div>
                )}
              </div>
              <div className="d-flex flex-wrap gap-4 justify-content-center">
                <Link href="/" className="btn btn-danger">
                  Return Home
                </Link>
                <button onClick={reset} className="btn btn-danger">
                  Try again
                </button>
              </div>
            </div>
          </div>
        </div>
      </div>
    </div>
  );
}
