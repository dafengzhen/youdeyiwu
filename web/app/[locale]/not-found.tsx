import Link from 'next/link';

export default function NotFound() {
  return (
    <div className="row mx-0">
      <div className="col">
        <div className="d-flex justify-content-center align-items-center min-h-screen">
          <div className="card border-0 text-center">
            <div className="card-body">
              <h5 className="card-title text-5xl font-bold">Not Found!</h5>
              <p className="card-text my-4">
                Could not find requested resource
              </p>
              <Link className="btn btn-primary" href="/">
                Return Home
              </Link>
            </div>
          </div>
        </div>
      </div>
    </div>
  );
}
