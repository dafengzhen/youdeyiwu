'use client';

import Box from '@/app/admin/common/box';
import { ChangeEvent, type FormEvent, useContext, useState } from 'react';
import { GlobalContext } from '@/app/contexts';
import { useMutation } from '@tanstack/react-query';
import { getUserAlias } from '@/app/common/client';
import { IUser } from '@/app/interfaces/users';
import UpdateStatesUserAction from '@/app/actions/users/update-states-user-action';

export default function UpdateStates({ user }: { user: IUser }) {
  const { toast } = useContext(GlobalContext);
  const [form, setForm] = useState<{
    accountNonExpired: boolean;
    credentialsNonExpired: boolean;
    accountNonLocked: boolean;
    enabled: boolean;
  }>({
    accountNonExpired: user.accountNonExpired ?? true,
    credentialsNonExpired: user.credentialsNonExpired ?? true,
    accountNonLocked: user.accountNonLocked ?? true,
    enabled: user.enabled ?? true,
  });

  const updateStatesUserActionMutation = useMutation({
    mutationFn: UpdateStatesUserAction,
  });

  async function onSubmit(e: FormEvent<HTMLFormElement>) {
    try {
      e.stopPropagation();
      e.preventDefault();

      const id = user.id;
      await updateStatesUserActionMutation.mutateAsync({ id, variables: form });

      toast.current.show({
        type: 'success',
        message: 'States updated successfully',
      });
    } catch (e: any) {
      updateStatesUserActionMutation.reset();
      toast.current.show({
        type: 'danger',
        message: e.message,
      });
    }
  }

  function onChangeForm(e: ChangeEvent<HTMLInputElement>) {
    const name = e.target.name;
    const value = e.target.checked;
    setForm({ ...form, [name]: value });
  }

  function onClickLabel(
    item:
      | 'accountNonExpired'
      | 'accountNonLocked'
      | 'credentialsNonExpired'
      | 'enabled',
  ) {
    setForm({ ...form, [item]: !form[item] });
  }

  return (
    <Box title={`${getUserAlias(user)} (ID. ${user.id})`}>
      <form className="vstack gap-4" onSubmit={onSubmit}>
        <div>
          <label className="form-label">States</label>
          <div className="form-control vstack gap-4 user-select-none">
            <div className="form-check form-switch">
              <input
                className="form-check-input"
                type="checkbox"
                role="switch"
                name="accountNonExpired"
                checked={form.accountNonExpired}
                onChange={onChangeForm}
              />
              <label
                className="form-check-label cursor-pointer"
                onClick={() => onClickLabel('accountNonExpired')}
              >
                Account Non Expired
              </label>
            </div>
            <div className="form-check form-switch">
              <input
                className="form-check-input"
                type="checkbox"
                role="switch"
                name="accountNonLocked"
                checked={form.accountNonLocked}
                onChange={onChangeForm}
              />
              <label
                className="form-check-label cursor-pointer"
                onClick={() => onClickLabel('accountNonLocked')}
              >
                Account Non Locked
              </label>
            </div>
            <div className="form-check form-switch">
              <input
                className="form-check-input"
                type="checkbox"
                role="switch"
                name="credentialsNonExpired"
                checked={form.credentialsNonExpired}
                onChange={onChangeForm}
              />
              <label
                className="form-check-label cursor-pointer"
                onClick={() => onClickLabel('credentialsNonExpired')}
              >
                Credentials Non Expired
              </label>
            </div>
            <div className="form-check form-switch">
              <input
                className="form-check-input"
                type="checkbox"
                role="switch"
                name="enabled"
                checked={form.enabled}
                onChange={onChangeForm}
              />
              <label
                className="form-check-label cursor-pointer"
                onClick={() => onClickLabel('enabled')}
              >
                Enabled
              </label>
            </div>
          </div>
          <div className="form-text">Set user account states</div>
        </div>

        <div>
          <button
            disabled={updateStatesUserActionMutation.isPending}
            type="submit"
            className="btn btn-success"
          >
            {updateStatesUserActionMutation.isPending
              ? 'Updating'
              : 'Update User States'}
          </button>
        </div>
      </form>
    </Box>
  );
}
