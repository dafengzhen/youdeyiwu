'use client';

import LoadMore from '@/app/home/load-more';
import Nodata from '@/app/common/nodata';

export default function Messages() {
  return (
    <div className="row mx-0">
      <div className="col">
        <div className="container">
          <div className="card border-0">
            <div className="card-header bg-transparent border-bottom-0 fw-bold">
              Messages
            </div>
            <div className="card-body p-0">
              <div className="d-flex flex-column gap-4">
                <Nodata />

                {/*<div className="card border-0 card-hover">*/}
                {/*  <div className="card-body py-2">*/}
                {/*    <div className="d-flex gap-4">*/}
                {/*      <div*/}
                {/*        className="d-flex justify-content-center align-items-center"*/}
                {/*        style={{ width: '2.5rem', height: '2.5rem' }}*/}
                {/*      >*/}
                {/*        <i className="bi bi-bell fs-2 d-flex"></i>*/}
                {/*      </div>*/}
                {/*      <div className="d-flex flex-column gap-2 flex-grow-1">*/}
                {/*        <div className="d-flex justify-content-between gap-4">*/}
                {/*          <Link href="">*/}
                {/*            <Image*/}
                {/*              className="rounded-circle object-fit-contain image-hover"*/}
                {/*              src="/avatar.png"*/}
                {/*              alt=""*/}
                {/*              width={40}*/}
                {/*              height={40}*/}
                {/*            />*/}
                {/*          </Link>*/}
                {/*          <div>*/}
                {/*            <i*/}
                {/*              className="bi bi-bookmark-check text-primary fs-4 cursor-pointer"*/}
                {/*              title="Set Read"*/}
                {/*            ></i>*/}
                {/*            <i*/}
                {/*              className="bi bi-trash text-danger fs-4 ms-2 cursor-pointer"*/}
                {/*              title="Delete"*/}
                {/*            ></i>*/}
                {/*          </div>*/}
                {/*        </div>*/}

                {/*        <div className="mt-2 d-flex flex-column gap-2">*/}
                {/*          <div>You have earned a new badge: Gone streaking</div>*/}
                {/*          <div>You maintained a streak for 2 days</div>*/}
                {/*          <div className="mt-2">*/}
                {/*            <button type="button" className="btn btn-primary">*/}
                {/*              View Details*/}
                {/*            </button>*/}
                {/*          </div>*/}
                {/*        </div>*/}
                {/*      </div>*/}
                {/*    </div>*/}
                {/*  </div>*/}
                {/*</div>*/}
                {/*<div className="card border-0 card-hover">*/}
                {/*  <div className="card-body py-2">*/}
                {/*    <div className="d-flex gap-4">*/}
                {/*      <div*/}
                {/*        className="d-flex justify-content-center align-items-center"*/}
                {/*        style={{ width: '2.5rem', height: '2.5rem' }}*/}
                {/*      >*/}
                {/*        <i className="bi bi-bell fs-2 d-flex"></i>*/}
                {/*      </div>*/}
                {/*      <div className="d-flex flex-column gap-2 flex-grow-1">*/}
                {/*        <div className="d-flex justify-content-between gap-4">*/}
                {/*          <Link href="">*/}
                {/*            <Image*/}
                {/*              className="rounded-circle object-fit-contain image-hover"*/}
                {/*              src="/avatar.png"*/}
                {/*              alt=""*/}
                {/*              width={40}*/}
                {/*              height={40}*/}
                {/*            />*/}
                {/*          </Link>*/}
                {/*          <div>*/}
                {/*            <i*/}
                {/*              className="bi bi-bookmark-check text-primary fs-4 cursor-pointer"*/}
                {/*              title="Set Read"*/}
                {/*            ></i>*/}
                {/*            <i*/}
                {/*              className="bi bi-trash text-danger fs-4 ms-2 cursor-pointer"*/}
                {/*              title="Delete"*/}
                {/*            ></i>*/}
                {/*          </div>*/}
                {/*        </div>*/}

                {/*        <div className="mt-2 d-flex flex-column gap-2">*/}
                {/*          <div>You have earned a new badge: Gone streaking</div>*/}
                {/*          <div>You maintained a streak for 2 days</div>*/}
                {/*          <div className="mt-2">*/}
                {/*            <button type="button" className="btn btn-primary">*/}
                {/*              View Details*/}
                {/*            </button>*/}
                {/*          </div>*/}
                {/*        </div>*/}
                {/*      </div>*/}
                {/*    </div>*/}
                {/*  </div>*/}
                {/*</div>*/}
                <LoadMore />
              </div>
            </div>
          </div>
        </div>
      </div>
    </div>
  );
}
