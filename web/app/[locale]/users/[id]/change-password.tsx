import type { TTabId } from '@/app/[locale]/users/[id]/userid';
import clsx from 'clsx';
import { type ChangeEvent, type FormEvent, useContext, useState } from 'react';
import { GlobalContext } from '@/app/[locale]/contexts';
import { useMutation } from '@tanstack/react-query';
import UpdatePasswordUserAction, {
  type IUpdatePasswordUserActionVariables,
} from '@/app/[locale]/actions/users/update-password-user-action';
import { trimObjectStrings } from '@/app/[locale]/common/client';
import type { IUserDetails } from '@/app/[locale]/interfaces/users';
import { useTranslations } from 'next-intl';

export default function ChangePassword({
  selectedTabIndex,
  details,
}: {
  selectedTabIndex?: TTabId;
  details: IUserDetails;
}) {
  const { toast } = useContext(GlobalContext);
  const [form, setForm] = useState<{
    username: string;
    oldPassword?: string;
    newPassword?: string;
  }>({
    username: details.username ?? '',
    oldPassword: '',
    newPassword: '',
  });
  const t = useTranslations();

  const updatePasswordUserActionMutation = useMutation({
    mutationFn: async (variables: {
      id: number;
      variables: IUpdatePasswordUserActionVariables;
    }) => {
      const response = await UpdatePasswordUserAction(variables);
      if (response.isError) {
        throw response;
      }
    },
  });

  async function onSubmit(e: FormEvent<HTMLFormElement>) {
    try {
      e.stopPropagation();
      e.preventDefault();

      const variables = trimObjectStrings({
        ...form,
      }) as IUpdatePasswordUserActionVariables & { username?: string };
      delete variables.username;

      if (!variables.oldPassword) {
        toast.current.show({
          type: 'danger',
          message: t('common.passwordCannotBeEmpty'),
        });
        return;
      }

      if (
        variables.oldPassword.length < 6 ||
        variables.oldPassword.length > 18
      ) {
        toast.current.show({
          type: 'danger',
          message: t('common.passwordFromText'),
        });
        return;
      }

      if (!variables.newPassword) {
        toast.current.show({
          type: 'danger',
          message: t('common.passwordCannotBeEmpty'),
        });
        return;
      }

      if (
        variables.newPassword.length < 6 ||
        variables.newPassword.length > 18
      ) {
        toast.current.show({
          type: 'danger',
          message: t('common.passwordFromText'),
        });
        return;
      }

      const id = details.id;
      await updatePasswordUserActionMutation.mutateAsync({
        id,
        variables,
      });

      toast.current.show({
        type: 'success',
        message: t('common.successfulUpdate'),
      });
    } catch (e: any) {
      updatePasswordUserActionMutation.reset();
      toast.current.show({
        type: 'danger',
        message: e.message,
      });
    }
  }

  function onChangeForm(e: ChangeEvent<HTMLInputElement>) {
    const name = e.target.name;
    const value = e.target.value;
    setForm({ ...form, [name]: value });
  }

  return (
    <div
      className={clsx('card', {
        'border-info': selectedTabIndex === 'ChangePassword',
      })}
    >
      <div className="card-body">
        <form className="vstack gap-4" onSubmit={onSubmit}>
          <div className="visually-hidden">
            <label className="form-label">Username</label>
            <input
              name="username"
              type="text"
              className="form-control"
              autoComplete="username"
              value={form.username}
              onChange={onChangeForm}
            />
          </div>

          <div>
            <label className="form-label">{t('common.oldPassword')}</label>
            <input
              required
              name="oldPassword"
              type="password"
              className="form-control"
              autoComplete="password"
              value={form.oldPassword}
              onChange={onChangeForm}
            />
            <div className="form-text">
              {t('common.passwordLengthFormText')}
            </div>
          </div>

          <div>
            <label className="form-label">{t('common.newPassword')}</label>
            <input
              required
              name="newPassword"
              type="password"
              className="form-control"
              autoComplete="new-password"
              value={form.newPassword}
              onChange={onChangeForm}
            />
            <div className="form-text">
              {t('common.passwordLengthFormText')}
            </div>
          </div>

          <div>
            <button
              disabled={updatePasswordUserActionMutation.isPending}
              type="submit"
              className="btn btn-success col-auto"
            >
              {updatePasswordUserActionMutation.isPending
                ? t('common.updating')
                : t('common.changePassword')}
            </button>
          </div>
        </form>
      </div>
    </div>
  );
}
