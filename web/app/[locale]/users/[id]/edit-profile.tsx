import type { TTabId } from '@/app/[locale]/users/[id]/userid';
import clsx from 'clsx';
import { type ChangeEvent, type FormEvent, useContext, useState } from 'react';
import type { IUserDetails } from '@/app/[locale]/interfaces/users';
import { useMutation } from '@tanstack/react-query';
import UpdateProfileUserAction, {
  type IUpdateProfileUserActionVariables,
} from '@/app/[locale]/actions/users/update-profile-user-action';
import { GlobalContext } from '@/app/[locale]/contexts';
import { trimObjectStrings } from '@/app/[locale]/common/client';

export default function EditProfile({
  selectedTabIndex,
  details,
}: {
  selectedTabIndex?: TTabId;
  details: IUserDetails;
}) {
  const { toast } = useContext(GlobalContext);
  const [form, setForm] = useState<{
    alias?: string;
    avatar?: string;
    oneSentence?: string;
  }>({
    alias: details.alias ?? '',
    avatar: details.avatar ?? '',
    oneSentence: details.oneSentence ?? '',
  });

  const updateProfileUserActionMutation = useMutation({
    mutationFn: async (variables: {
      id: number;
      variables: IUpdateProfileUserActionVariables;
    }) => {
      const response = await UpdateProfileUserAction(variables);
      if (response.isError) {
        throw response;
      }
    },
  });

  async function onSubmit(e: FormEvent<HTMLFormElement>) {
    try {
      e.stopPropagation();
      e.preventDefault();

      const id = details.id;
      await updateProfileUserActionMutation.mutateAsync({
        id,
        variables: trimObjectStrings({ ...form }),
      });

      toast.current.show({
        type: 'success',
        message: 'Profile updated successfully',
      });
    } catch (e: any) {
      updateProfileUserActionMutation.reset();
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
        'border-info': selectedTabIndex === 'EditProfile',
      })}
    >
      <div className="card-body">
        <form className="vstack gap-4" onSubmit={onSubmit}>
          <div>
            <label className="form-label">Alias</label>
            <input
              type="text"
              className="form-control"
              name="alias"
              value={form.alias}
              onChange={onChangeForm}
              aria-describedby="alias"
            />
            <div className="form-text">
              Display aliases first, followed by usernames
            </div>
          </div>

          <div>
            <label className="form-label">Avatar</label>
            <input
              type="text"
              className="form-control"
              name="avatar"
              value={form.avatar}
              onChange={onChangeForm}
              aria-describedby="avatar"
            />
            <div className="form-text">
              The recommended size for the avatar image is 260 x 260 or 200 x
              200
            </div>
            <div className="form-text">
              Only cover URLs using the HTTP or HTTPS protocol are supported
            </div>
          </div>

          <div>
            <label className="form-label">One Sentence</label>
            <input
              type="text"
              className="form-control"
              name="oneSentence"
              value={form.oneSentence}
              onChange={onChangeForm}
              aria-describedby="oneSentence"
            />
            <div className="form-text">
              A brief description of what I want to describe
            </div>
          </div>

          <div>
            <button
              disabled={updateProfileUserActionMutation.isPending}
              type="submit"
              className="btn btn-success col-auto"
            >
              {updateProfileUserActionMutation.isPending
                ? 'Updating'
                : 'Edit Profile'}
            </button>
          </div>
        </form>
      </div>
    </div>
  );
}
