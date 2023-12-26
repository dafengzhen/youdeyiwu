'use client';

import { useRouter } from 'next/navigation';
import Link from 'next/link';

export default function ClientErrorHandler({ message }: { message?: string }) {
  const router = useRouter();

  function back() {
    router.back();
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
                {message ? (
                  <div className="">
                    <span>
                      === <span className="fw-bold">Details</span> ===
                    </span>
                    &nbsp;
                    <span>{message}</span>
                  </div>
                ) : (
                  <div className="">Unknown Error</div>
                )}
              </div>
              <div className="d-flex flex-wrap gap-4 justify-content-center">
                <Link href="/" className="btn btn-danger" role="button">
                  To homepage
                </Link>
                <button type="button" onClick={back} className="btn btn-danger">
                  Go back
                </button>
              </div>
            </div>
          </div>
        </div>
      </div>
    </div>
  );
}
