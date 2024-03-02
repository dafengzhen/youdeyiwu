'use client';

import type { IUser } from '@/app/interfaces/users';
import { type ChangeEvent, type FormEvent, useContext, useState } from 'react';
import Link from 'next/link';
import { useMutation } from '@tanstack/react-query';
import { GlobalContext } from '@/app/contexts';
import { getUserAlias, trimObjectStrings } from '@/app/common/client';
import UpdateRootConfigAction, {
  type IUpdateRootActionVariables,
} from '@/app/actions/configs/root/update-root-config-action';

export default function InitRoot({
  currentUser,
}: {
  currentUser: IUser | null;
}) {
  const { toast } = useContext(GlobalContext);
  const [form, setForm] = useState({
    secret: '',
  });
  const [finish, setFinish] = useState(false);
  const isLogin = !!currentUser;

  const updateRootConfigActionMutation = useMutation({
    mutationFn: async (variables: IUpdateRootActionVariables) => {
      const response = await UpdateRootConfigAction(variables);
      if (response.isError) {
        throw response;
      }
    },
  });

  async function onSubmit(e: FormEvent<HTMLFormElement>) {
    try {
      e.stopPropagation();
      e.preventDefault();

      if (!isLogin) {
        toast.current.show({
          type: 'danger',
          message: 'Not logged in yet. Please login before proceeding',
        });
        return;
      }

      const variables = trimObjectStrings({
        ...form,
      }) as IUpdateRootActionVariables;
      const secret = variables.secret;

      if (!secret) {
        toast.current.show({
          type: 'danger',
          message: 'Please enter the root secret',
        });
        return;
      }

      await updateRootConfigActionMutation.mutateAsync(variables);
      setFinish(true);

      toast.current.show({
        type: 'success',
        message: `Congratulations! User ${getUserAlias(currentUser)} (ID. ${
          currentUser.id
        }) has become a forum administrator`,
      });
    } catch (e: any) {
      updateRootConfigActionMutation.reset();
      toast.current.show({
        type: 'danger',
        message: e.message,
      });
    }
  }

  function onClickReturn() {
    location.assign('/');
  }

  function onChangeForm(e: ChangeEvent<HTMLInputElement>) {
    const name = e.target.name;
    const value = e.target.value;
    setForm({ ...form, [name]: value });
  }

  return (
    <div className="row mx-0">
      <div className="col">
        <div className="container text-center py-5">
          <div className="py-5 my-4">
            <div className="mb-5">
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
            <div className="mb-5">
              <form className="vstack gap-4" onSubmit={onSubmit}>
                <div className="text-start">
                  <label className="form-label">
                    <span className="text-danger fw-bold">*</span>
                    Secret
                  </label>
                  <input
                    disabled={
                      updateRootConfigActionMutation.isPending || finish
                    }
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
                  <div className="form-text">
                    The secret can only be used once, and once used, the key
                    becomes invalid. The new password will be stored in the
                    database configuration
                  </div>
                </div>

                <div className="my-4">
                  {finish ? (
                    <button
                      onClick={onClickReturn}
                      type="button"
                      className="btn btn-secondary"
                    >
                      Return Homepage
                    </button>
                  ) : (
                    <button
                      disabled={updateRootConfigActionMutation.isPending}
                      type="submit"
                      className="btn btn-primary"
                    >
                      {updateRootConfigActionMutation.isPending
                        ? 'Initializing'
                        : 'Initialize'}
                    </button>
                  )}
                </div>
              </form>
            </div>
          </div>
        </div>
      </div>
    </div>
  );
}
