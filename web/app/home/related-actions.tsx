import Link from 'next/link';

export default function RelatedActions({
  isLogin,
  editPostId,
}: {
  isLogin?: boolean;
  editPostId?: number;
}) {
  return (
    <div className="card border-0 shadow-sm">
      <div className="card-header bg-transparent border-bottom-0 fw-bold">
        Related Actions
      </div>
      <div className="card-body p-0">
        <div className="card border-0">
          <div className="card-body py-2">
            <div className="d-flex flex-column gap-2">
              {editPostId ? (
                <Link
                  href={`/posts/${editPostId}/edit`}
                  className="btn rounded-2 btn-primary w-100 d-flex justify-content-center"
                >
                  <div
                    className="text-start flex-shrink-0 text-truncate"
                    style={{ width: 140 }}
                  >
                    <i className="bi bi-pencil-square me-2"></i>
                    Edit Article
                  </div>
                </Link>
              ) : (
                <Link
                  href="/posts/new"
                  className="btn rounded-2 btn-primary w-100 d-flex justify-content-center"
                >
                  <div
                    className="text-start flex-shrink-0 text-truncate"
                    style={{ width: 140 }}
                  >
                    <i className="bi bi-pen me-2"></i>
                    Create Article
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
                      Login Now
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
                      Quick Register
                    </div>
                  </Link>
                </>
              )}
            </div>
          </div>
        </div>
      </div>
    </div>
  );
}
