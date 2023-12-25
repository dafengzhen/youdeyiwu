'use client';

import type { IUser } from '@/app/interfaces/users';
import { type ChangeEvent, useState } from 'react';
import Link from 'next/link';

export default function InitRoot({
  currentUser,
}: {
  currentUser: IUser | null;
}) {
  const [form, setForm] = useState({
    secret: '',
  });

  function onChangeForm(e: ChangeEvent<HTMLInputElement>) {
    const name = e.target.name;
    const value = e.target.value;
    setForm({ ...form, [name]: value });
  }

  return (
    <div className="row mx-0">
      <div className="col">
        <div className="container text-center py-5 my-4">
          <div className="p-5 my-4">
            <div className="mb-4">
              <div>
                <h2 className="fw-bold display-6">
                  Initialize Forum Administrator
                </h2>
              </div>
            </div>
            <div className="mb-5">
              <div>
                <p className="lead">
                  To initiate initialization, you need to enter a specific
                  secret, which can be found in the console output when the
                  server program starts.
                  <br />
                  Note that it will only be output to the console during the
                  initial initialization. Subsequently, it will be stored in the
                  database, so please do not disclose it.
                </p>
              </div>
            </div>
            <div className="mb-5 container w-50">
              <form className="vstack gap-4 text-start">
                <div>
                  <label className="form-label">Secret</label>
                  <input
                    required
                    type="text"
                    className="form-control"
                    name="secret"
                    value={form.secret}
                    onChange={onChangeForm}
                    placeholder="Please enter the root secret"
                    aria-describedby="secret"
                  />
                  <div className="form-text">
                    Prior to initialization, if you haven&apos;t logged in yet,
                    please <Link href="/login">login</Link> first. If you
                    don&apos;t have an account yet, please{' '}
                    <Link href="/register">register</Link> an account of your
                    choice
                  </div>
                </div>
              </form>
            </div>
            <div className="mb-2">
              <button type="button" className="btn btn-primary">
                Initialize
              </button>
            </div>
          </div>
        </div>
      </div>
    </div>
  );
}
