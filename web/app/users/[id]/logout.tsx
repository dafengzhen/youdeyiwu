import { TTabId } from '@/app/users/[id]/userid';
import clsx from 'clsx';
import { useContext } from 'react';
import { GlobalContext } from '@/app/contexts';
import { useMutation } from '@tanstack/react-query';
import LogoutUserAction from '@/app/actions/users/logout-user-action';
import { IUserDetails } from '@/app/interfaces/users';

export default function Logout({
  selectedTabIndex,
  details,
}: {
  selectedTabIndex?: TTabId;
  details: IUserDetails;
}) {
  const { toast } = useContext(GlobalContext);

  const logoutUserActionMutation = useMutation({
    mutationFn: LogoutUserAction,
  });

  async function onClickLogout() {
    try {
      const id = details?.id;
      await logoutUserActionMutation.mutateAsync({
        id,
      });

      toast.current.show({
        type: 'success',
        message: 'Logout successfully',
      });
    } catch (e: any) {
      logoutUserActionMutation.reset();
      toast.current.show({
        type: 'danger',
        message: e.message,
      });
    }
  }

  return (
    <div
      className={clsx('card', {
        'border-info': selectedTabIndex === 'Logout',
      })}
    >
      <div className="card-body">
        <div className="form-text mb-4">Are you sure you want to logout?</div>

        <div>
          <button
            disabled={logoutUserActionMutation.isPending}
            onClick={onClickLogout}
            type="submit"
            className="btn btn-danger col-auto"
          >
            {logoutUserActionMutation.isPending ? 'Logging out' : 'Logout'}
          </button>
        </div>
      </div>
    </div>
  );
}
