'use client';

import Link from 'next/link';
import Username from '@/app/[locale]/login/username';
import { useTranslations } from 'next-intl';

export default function Login() {
  const t = useTranslations();

  return (
    <div className="row mx-0">
      <div className="col">
        <div className="container">
          <div className="row">
            <div className="col pe-0 overflow-hidden">
              <div className="card rounded-start-4 border-0 h-100">
                <div className="card-body ps-4 py-5 my-5 animate__animated animate__fadeInLeft">
                  <div className="mb-5 hstack gap-2 justify-content-end">
                    <div className="cursor-default">
                      {t('common.dontHaveAnAccountYet')}
                    </div>
                    <div>
                      <Link
                        className="btn yw-btn-outline-light"
                        href="/register"
                      >
                        {t('common.startRegistration')}
                      </Link>
                    </div>
                  </div>

                  <div className="fs-4 text-center fw-bold">
                    {t('common.loginToYourAccount')}
                  </div>
                </div>
              </div>
            </div>
            <div className="col-auto overflow-hidden px-0">
              <div className="card border-0 h-100">
                <div className="card-body d-flex align-items-center position-relative py-0">
                  <div className="d-flex position-absolute start-0 end-0 top-0 h-100 py-5 justify-content-center">
                    <div className="vr text-secondary text-opacity-75"></div>
                  </div>
                  <div className="bg-body z-1 border border-secondary border-opacity-25 rounded-3 text-secondary px-2 py-3">
                    {t('common.or')}
                  </div>
                </div>
              </div>
            </div>
            <div className="col ps-0 overflow-hidden">
              <div className="card rounded-end-4 border-0 h-100">
                <div className="card-body py-5 pe-4 vstack gap-4 my-5 animate__animated animate__fadeInRight">
                  <Username />
                </div>
              </div>
            </div>
          </div>
        </div>
      </div>
    </div>
  );
}
