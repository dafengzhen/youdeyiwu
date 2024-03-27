'use client';

import type { IUser } from '@/app/[locale]/interfaces/users';
import { type ChangeEvent, type FormEvent, useContext, useState } from 'react';
import { useMutation } from '@tanstack/react-query';
import { GlobalContext } from '@/app/[locale]/contexts';
import { getUserAlias, trimObjectStrings } from '@/app/[locale]/common/client';
import UpdateRootConfigAction, {
  type IUpdateRootActionVariables,
} from '@/app/[locale]/actions/configs/root/update-root-config-action';
import { useTranslations } from 'next-intl';

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
  const t = useTranslations();

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
          message: t('common.pleaseLoginBeforeContinuing'),
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
          message: t('common.theKeyCannotBeEmpty'),
        });
        return;
      }

      await updateRootConfigActionMutation.mutateAsync(variables);
      setFinish(true);

      const alias = `${getUserAlias(currentUser)} (ID. ${currentUser.id})`;
      toast.current.show({
        type: 'success',
        message: t('common.forumAdministratorInitialisationMsg', {
          alias,
        }),
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
                  {t('common.forumAdministratorInitialisation')}
                </h2>
              </div>
            </div>
            <div className="mb-5">
              <div>
                <p className="lead">
                  {t('common.forumAdministratorInitialisationFormText1')}
                </p>
              </div>
            </div>
            <div className="mb-5">
              <form className="vstack gap-4" onSubmit={onSubmit}>
                <div className="text-start">
                  <label className="form-label">
                    <span className="text-danger fw-bold">*</span>
                    Key
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
                    aria-describedby="secret"
                  />
                  <div className="form-text">
                    {t('common.forumAdministratorInitialisationFormText2')}
                  </div>
                  <div className="form-text">
                    {t('common.forumAdministratorInitialisationFormText3')}
                  </div>
                </div>

                <div className="my-4">
                  {finish ? (
                    <button
                      onClick={onClickReturn}
                      type="button"
                      className="btn btn-secondary"
                    >
                      {t('common.backToHomepage')}
                    </button>
                  ) : (
                    <button
                      disabled={updateRootConfigActionMutation.isPending}
                      type="submit"
                      className="btn btn-primary"
                    >
                      {updateRootConfigActionMutation.isPending
                        ? t('common.inProgress')
                        : t('common.submit')}
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
