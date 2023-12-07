import { TTabId } from '@/app/users/[id]/userid';
import clsx from 'clsx';
import { ChangeEvent, FormEvent, useContext, useState } from 'react';
import { GlobalContext } from '@/app/contexts';
import { useMutation } from '@tanstack/react-query';
import UpdatePasswordUserAction, {
  IUpdatePasswordUserActionVariables,
} from '@/app/actions/users/update-password-user-action';
import { trimObjectStrings } from '@/app/common/client';
import { IUserDetails } from '@/app/interfaces/users';

export default function ChangePassword({
  selectedTabIndex,
  details,
}: {
  selectedTabIndex?: TTabId;
  details: IUserDetails;
}) {
  const { toast } = useContext(GlobalContext);
  const [form, setForm] = useState<{
    oldPassword?: string;
    newPassword?: string;
  }>({
    oldPassword: '',
    newPassword: '',
  });

  const updatePasswordUserActionMutation = useMutation({
    mutationFn: UpdatePasswordUserAction,
  });

  async function onSubmit(e: FormEvent<HTMLFormElement>) {
    try {
      e.stopPropagation();
      e.preventDefault();

      const variables = trimObjectStrings({
        ...form,
      }) as IUpdatePasswordUserActionVariables;

      if (!variables.oldPassword) {
        toast.current.show({
          type: 'danger',
          message: 'Old Password can not be empty',
        });
        return;
      }

      if (
        variables.oldPassword.length < 6 ||
        variables.oldPassword.length > 18
      ) {
        toast.current.show({
          type: 'danger',
          message: 'Old Password length should be between 6 and 18 characters',
        });
        return;
      }

      if (!variables.newPassword) {
        toast.current.show({
          type: 'danger',
          message: 'New Password can not be empty',
        });
        return;
      }

      if (
        variables.newPassword.length < 6 ||
        variables.newPassword.length > 18
      ) {
        toast.current.show({
          type: 'danger',
          message: 'New Password length should be between 6 and 18 characters',
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
        message: 'Password updated successfully',
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
          <div>
            <label className="form-label">Old Password</label>
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
              Password length should be between 6 and 18 characters
            </div>
          </div>

          <div>
            <label className="form-label">New Password</label>
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
              Password length should be between 6 and 18 characters
            </div>
          </div>

          <div>
            <button
              disabled={updatePasswordUserActionMutation.isPending}
              type="submit"
              className="btn btn-outline-success col-auto"
            >
              {updatePasswordUserActionMutation.isPending
                ? 'Updating'
                : 'Change Password'}
            </button>
          </div>
        </form>
      </div>
    </div>
  );
}
