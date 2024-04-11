export default async function BottomNavH5() {
  return (
    <div
      className="d-md-none container-fluid position-relative position-fixed start-50 translate-middle-x"
      style={{ bottom: '1.5rem' }}
    >
      <div className="shadow rounded rounded-bottom-5 rounded-top-4 text-bg-dark px-2 py-2 d-flex align-items-center justify-content-between">
        <div className="d-flex align-items-center gap-2 ms-3">
          <div>
            <button type="button" className="btn btn-dark rounded-circle">
              <i className="bi bi-house-door-fill fs-2"></i>
            </button>
          </div>
          <div>
            <button type="button" className="btn btn-dark rounded-circle">
              <i className="bi bi-box fs-2"></i>
            </button>
          </div>
        </div>
        <div className="d-flex align-items-center gap-2 me-3">
          <div>
            <button type="button" className="btn btn-dark rounded-circle">
              <i className="bi bi-envelope fs-2"></i>
            </button>
          </div>
          <div>
            <button type="button" className="btn btn-dark rounded-circle">
              <i className="bi bi-person fs-2"></i>
            </button>
          </div>
        </div>
      </div>

      <div
        className="shadow rounded-circle bg-white position-absolute top-0 start-50 translate-middle d-flex align-items-center justify-content-center"
        style={{ width: 64, height: 64 }}
      >
        <div
          className="rounded-circle text-bg-dark d-flex align-items-center justify-content-center"
          style={{ width: 48, height: 48 }}
        >
          <div className="">
            <button type="button" className="btn btn-dark rounded-circle">
              <i className="bi bi-pen fs-2"></i>
            </button>
          </div>
        </div>
      </div>
    </div>
  );
}
